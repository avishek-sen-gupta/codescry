"""Integration point detection from CTags output.

Detects system integration points (HTTP, SOAP, messaging, sockets, database)
from code symbols using text search and regex heuristics.
"""

import re
from dataclasses import dataclass
from itertools import chain
from typing import Iterator

from .ctags import CTagsEntry, CTagsResult


@dataclass(frozen=True)
class IntegrationPoint:
    """A detected integration point."""

    entry: CTagsEntry
    integration_type: str  # "http_rest", "soap", "messaging", "socket", "database"
    confidence: str  # "high", "medium", "low"
    matched_pattern: str  # The pattern that triggered detection


@dataclass(frozen=True)
class IntegrationDetectorResult:
    """Result of integration detection."""

    integration_points: list[IntegrationPoint]
    entries_scanned: int


# Pattern dictionaries for each integration type
# Each category has patterns for name, signature, and scope matching
INTEGRATION_PATTERNS: dict[str, dict[str, tuple[str, ...]]] = {
    "http_rest": {
        "name_patterns": (
            r"(?i)http",
            r"(?i)rest(?:ful)?",
            r"(?i)api(?:client|handler|controller)?",
            r"(?i)endpoint",
            r"(?i)web(?:client|service)",
            r"(?i)fetch",
            r"(?i)request(?:handler|mapping)?",
            r"(?i)response",
            r"(?i)get(?:mapping|request)",
            r"(?i)post(?:mapping|request)",
            r"(?i)put(?:mapping|request)",
            r"(?i)delete(?:mapping|request)",
            r"(?i)patch(?:mapping|request)",
        ),
        "signature_patterns": (
            r"@RequestMapping",
            r"@GetMapping",
            r"@PostMapping",
            r"@PutMapping",
            r"@DeleteMapping",
            r"@PatchMapping",
            r"@RestController",
            r"@Controller",
            r"@RequestBody",
            r"@ResponseBody",
            r"@PathVariable",
            r"@RequestParam",
            r"HttpServletRequest",
            r"HttpServletResponse",
            r"@GET",
            r"@POST",
            r"@PUT",
            r"@DELETE",
            r"@Path\(",
            r"@Produces",
            r"@Consumes",
        ),
        "scope_patterns": (
            r"(?i)controller",
            r"(?i)resource",
            r"(?i)restcontroller",
            r"(?i)apicontroller",
            r"(?i)webcontroller",
        ),
    },
    "soap": {
        "name_patterns": (
            r"(?i)soap",
            r"(?i)wsdl",
            r"(?i)xml(?:service|client|handler)",
            r"(?i)webservice",
            r"(?i)porttype",
            r"(?i)binding",
            r"(?i)envelope",
            r"(?i)soapaction",
        ),
        "signature_patterns": (
            r"@WebService",
            r"@WebMethod",
            r"@WebParam",
            r"@SOAPBinding",
            r"@WebResult",
            r"SOAPMessage",
            r"SOAPEnvelope",
            r"SOAPBody",
            r"SOAPHeader",
            r"JAXBContext",
            r"Marshaller",
            r"Unmarshaller",
        ),
        "scope_patterns": (
            r"(?i)soapservice",
            r"(?i)webserviceimpl",
            r"(?i)portimpl",
        ),
    },
    "messaging": {
        "name_patterns": (
            r"(?i)kafka",
            r"(?i)rabbit(?:mq)?",
            r"(?i)jms",
            r"(?i)queue",
            r"(?i)topic",
            r"(?i)message(?:handler|listener|producer|consumer|sender|receiver)?",
            r"(?i)publisher",
            r"(?i)subscriber",
            r"(?i)event(?:handler|listener|publisher|bus)?",
            r"(?i)amqp",
            r"(?i)activemq",
            r"(?i)pulsar",
            r"(?i)nats",
            r"(?i)redis(?:pub|sub)?",
        ),
        "signature_patterns": (
            r"@KafkaListener",
            r"@JmsListener",
            r"@RabbitListener",
            r"@StreamListener",
            r"@SendTo",
            r"@EnableKafka",
            r"@EnableJms",
            r"@EnableRabbit",
            r"KafkaTemplate",
            r"JmsTemplate",
            r"RabbitTemplate",
            r"MessageChannel",
            r"MessageListener",
            r"MessageProducer",
            r"MessageConsumer",
            r"ConnectionFactory",
        ),
        "scope_patterns": (
            r"(?i)kafkaconsumer",
            r"(?i)kafkaproducer",
            r"(?i)messagelistener",
            r"(?i)eventhandler",
            r"(?i)subscriber",
        ),
    },
    "socket": {
        "name_patterns": (
            r"(?i)socket",
            r"(?i)websocket",
            r"(?i)tcp",
            r"(?i)udp",
            r"(?i)netty",
            r"(?i)nio",
            r"(?i)channel",
            r"(?i)serverchannel",
            r"(?i)socketchannel",
        ),
        "signature_patterns": (
            r"@ServerEndpoint",
            r"@OnOpen",
            r"@OnClose",
            r"@OnMessage",
            r"@OnError",
            r"@EnableWebSocket",
            r"WebSocketHandler",
            r"WebSocketSession",
            r"ServerSocket",
            r"DatagramSocket",
            r"SocketChannel",
            r"ServerSocketChannel",
            r"Selector",
            r"SelectionKey",
        ),
        "scope_patterns": (
            r"(?i)websockethandler",
            r"(?i)socketserver",
            r"(?i)socketclient",
            r"(?i)channelhandler",
        ),
    },
    "database": {
        "name_patterns": (
            r"(?i)jdbc",
            r"(?i)repository",
            r"(?i)dao",
            r"(?i)database",
            r"(?i)datasource",
            r"(?i)entitymanager",
            r"(?i)session(?:factory)?",
            r"(?i)transaction",
            r"(?i)query",
            r"(?i)crud",
            r"(?i)persist",
            r"(?i)hibernate",
            r"(?i)mybatis",
            r"(?i)jpa",
        ),
        "signature_patterns": (
            r"@Repository",
            r"@Entity",
            r"@Table",
            r"@Query",
            r"@Transactional",
            r"@PersistenceContext",
            r"@Id",
            r"@Column",
            r"@JoinColumn",
            r"@OneToMany",
            r"@ManyToOne",
            r"@ManyToMany",
            r"@OneToOne",
            r"Connection",
            r"PreparedStatement",
            r"ResultSet",
            r"Statement",
            r"DataSource",
            r"EntityManager",
            r"SessionFactory",
            r"JdbcTemplate",
            r"NamedParameterJdbcTemplate",
        ),
        "scope_patterns": (
            r"(?i)repository",
            r"(?i)dao",
            r"(?i)mapper",
            r"(?i)entitymanager",
        ),
    },
}

# Patterns that indicate high confidence (stronger signals)
HIGH_CONFIDENCE_PATTERNS: dict[str, tuple[str, ...]] = {
    "http_rest": (
        r"@RequestMapping",
        r"@GetMapping",
        r"@PostMapping",
        r"@RestController",
        r"@Controller",
        r"@GET",
        r"@POST",
        r"@Path\(",
    ),
    "soap": (
        r"@WebService",
        r"@WebMethod",
        r"SOAPMessage",
        r"SOAPEnvelope",
    ),
    "messaging": (
        r"@KafkaListener",
        r"@JmsListener",
        r"@RabbitListener",
        r"KafkaTemplate",
        r"JmsTemplate",
        r"RabbitTemplate",
    ),
    "socket": (
        r"@ServerEndpoint",
        r"@OnOpen",
        r"@OnMessage",
        r"WebSocketHandler",
        r"ServerSocket",
    ),
    "database": (
        r"@Repository",
        r"@Entity",
        r"@Query",
        r"@Transactional",
        r"EntityManager",
        r"JdbcTemplate",
        r"PreparedStatement",
    ),
}

# Strong keywords that indicate medium confidence for name matches
STRONG_KEYWORDS: tuple[str, ...] = (
    "controller",
    "repository",
    "listener",
    "handler",
    "template",
)


def first_matching_pattern(text: str | None, patterns: tuple[str, ...]) -> str | None:
    """Find the first pattern that matches the text.

    Args:
        text: The text to check (can be None).
        patterns: Tuple of regex patterns to match against.

    Returns:
        The first matched pattern string, or None if no match.
    """
    if text is None:
        return None

    return next(
        (pattern for pattern in patterns if re.search(pattern, text)),
        None,
    )


def _is_high_confidence_pattern(integration_type: str, matched_pattern: str) -> bool:
    """Check if the matched pattern is a high confidence pattern."""
    high_patterns = HIGH_CONFIDENCE_PATTERNS.get(integration_type, ())
    return any(re.search(hp, matched_pattern) for hp in high_patterns)


def _has_strong_keyword(matched_pattern: str) -> bool:
    """Check if the pattern contains a strong keyword."""
    pattern_lower = matched_pattern.lower()
    return any(kw in pattern_lower for kw in STRONG_KEYWORDS)


def determine_confidence(
    integration_type: str,
    matched_pattern: str,
    is_name_match: bool,
    is_signature_match: bool,
    is_scope_match: bool,
) -> str:
    """Determine confidence level based on match characteristics.

    Args:
        integration_type: The type of integration detected.
        matched_pattern: The pattern that triggered the match.
        is_name_match: Whether the match was on the symbol name.
        is_signature_match: Whether the match was on the signature.
        is_scope_match: Whether the match was on the scope.

    Returns:
        Confidence level: "high", "medium", or "low".
    """
    if _is_high_confidence_pattern(integration_type, matched_pattern):
        return "high"

    if is_signature_match:
        return "high"

    if is_scope_match:
        return "medium"

    if is_name_match and _has_strong_keyword(matched_pattern):
        return "medium"

    return "low"


def _check_field(
    text: str | None,
    patterns: tuple[str, ...],
    integration_type: str,
    field_name: str,
    is_name: bool = False,
    is_signature: bool = False,
    is_scope: bool = False,
) -> Iterator[tuple[str, str, str]]:
    """Check a single field against patterns and yield matches.

    Args:
        text: The text to check.
        patterns: Patterns to match against.
        integration_type: The integration type being checked.
        field_name: Name of the field for the matched_pattern prefix.
        is_name: Whether this is a name field match.
        is_signature: Whether this is a signature field match.
        is_scope: Whether this is a scope field match.

    Yields:
        Tuples of (integration_type, confidence, matched_pattern).
    """
    matched = first_matching_pattern(text, patterns)
    if matched is not None:
        confidence = determine_confidence(
            integration_type, matched, is_name, is_signature, is_scope
        )
        yield (integration_type, confidence, f"{field_name}:{matched}")


def _classify_for_integration_type(
    entry: CTagsEntry,
    integration_type: str,
    patterns: dict[str, tuple[str, ...]],
) -> Iterator[tuple[str, str, str]]:
    """Classify an entry for a single integration type.

    Args:
        entry: The CTags entry to classify.
        integration_type: The integration type to check.
        patterns: The patterns dict for this integration type.

    Yields:
        Tuples of (integration_type, confidence, matched_pattern).
    """
    yield from _check_field(
        entry.name,
        patterns["name_patterns"],
        integration_type,
        "name",
        is_name=True,
    )
    yield from _check_field(
        entry.signature,
        patterns["signature_patterns"],
        integration_type,
        "signature",
        is_signature=True,
    )
    yield from _check_field(
        entry.scope,
        patterns["scope_patterns"],
        integration_type,
        "scope",
        is_scope=True,
    )


def classify_entry(entry: CTagsEntry) -> list[tuple[str, str, str]]:
    """Classify a single CTags entry into integration types.

    Args:
        entry: The CTags entry to classify.

    Returns:
        List of (integration_type, confidence, matched_pattern) tuples.
    """
    return list(
        chain.from_iterable(
            _classify_for_integration_type(entry, integration_type, patterns)
            for integration_type, patterns in INTEGRATION_PATTERNS.items()
        )
    )


def _entry_to_integration_points(
    entry: CTagsEntry,
) -> Iterator[IntegrationPoint]:
    """Convert a CTags entry to integration points.

    Args:
        entry: The CTags entry to process.

    Yields:
        IntegrationPoint instances for each classification match.
    """
    return (
        IntegrationPoint(
            entry=entry,
            integration_type=integration_type,
            confidence=confidence,
            matched_pattern=matched_pattern,
        )
        for integration_type, confidence, matched_pattern in classify_entry(entry)
    )


def detect_integrations(result: CTagsResult) -> IntegrationDetectorResult:
    """Detect integration points from CTags result.

    Args:
        result: The CTags result containing code symbols.

    Returns:
        IntegrationDetectorResult with detected integration points.
    """
    integration_points = list(
        chain.from_iterable(
            _entry_to_integration_points(entry) for entry in result.entries
        )
    )

    return IntegrationDetectorResult(
        integration_points=integration_points,
        entries_scanned=len(result.entries),
    )
