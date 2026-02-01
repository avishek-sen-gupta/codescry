"""Integration point detection from CTags output.

Detects system integration points (HTTP, SOAP, messaging, sockets, database)
from code symbols using text search and regex heuristics.
"""

import re
from dataclasses import dataclass
from enum import Enum
from itertools import chain
from typing import Iterator

from .ctags import CTagsEntry, CTagsResult


class IntegrationType(Enum):
    """Types of system integrations that can be detected."""

    HTTP_REST = "http_rest"
    SOAP = "soap"
    MESSAGING = "messaging"
    SOCKET = "socket"
    DATABASE = "database"


class Confidence(Enum):
    """Confidence levels for integration point detection."""

    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


@dataclass(frozen=True)
class IntegrationPoint:
    """A detected integration point."""

    entry: CTagsEntry
    integration_type: IntegrationType
    confidence: Confidence
    matched_pattern: str


@dataclass(frozen=True)
class IntegrationDetectorResult:
    """Result of integration detection."""

    integration_points: list[IntegrationPoint]
    entries_scanned: int


# Pattern dictionaries for each integration type
# Each category has patterns for name, signature, and scope matching
INTEGRATION_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "name_patterns": [
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
        ],
        "signature_patterns": [
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
        ],
        "scope_patterns": [
            r"(?i)controller",
            r"(?i)resource",
            r"(?i)restcontroller",
            r"(?i)apicontroller",
            r"(?i)webcontroller",
        ],
    },
    IntegrationType.SOAP: {
        "name_patterns": [
            r"(?i)soap",
            r"(?i)wsdl",
            r"(?i)xml(?:service|client|handler)",
            r"(?i)webservice",
            r"(?i)porttype",
            r"(?i)binding",
            r"(?i)envelope",
            r"(?i)soapaction",
        ],
        "signature_patterns": [
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
        ],
        "scope_patterns": [
            r"(?i)soapservice",
            r"(?i)webserviceimpl",
            r"(?i)portimpl",
        ],
    },
    IntegrationType.MESSAGING: {
        "name_patterns": [
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
        ],
        "signature_patterns": [
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
        ],
        "scope_patterns": [
            r"(?i)kafkaconsumer",
            r"(?i)kafkaproducer",
            r"(?i)messagelistener",
            r"(?i)eventhandler",
            r"(?i)subscriber",
        ],
    },
    IntegrationType.SOCKET: {
        "name_patterns": [
            r"(?i)socket",
            r"(?i)websocket",
            r"(?i)tcp",
            r"(?i)udp",
            r"(?i)netty",
            r"(?i)nio",
            r"(?i)channel",
            r"(?i)serverchannel",
            r"(?i)socketchannel",
        ],
        "signature_patterns": [
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
        ],
        "scope_patterns": [
            r"(?i)websockethandler",
            r"(?i)socketserver",
            r"(?i)socketclient",
            r"(?i)channelhandler",
        ],
    },
    IntegrationType.DATABASE: {
        "name_patterns": [
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
        ],
        "signature_patterns": [
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
        ],
        "scope_patterns": [
            r"(?i)repository",
            r"(?i)dao",
            r"(?i)mapper",
            r"(?i)entitymanager",
        ],
    },
}

# Patterns that indicate high confidence (stronger signals)
HIGH_CONFIDENCE_PATTERNS = {
    IntegrationType.HTTP_REST: [
        r"@RequestMapping",
        r"@GetMapping",
        r"@PostMapping",
        r"@RestController",
        r"@Controller",
        r"@GET",
        r"@POST",
        r"@Path\(",
    ],
    IntegrationType.SOAP: [
        r"@WebService",
        r"@WebMethod",
        r"SOAPMessage",
        r"SOAPEnvelope",
    ],
    IntegrationType.MESSAGING: [
        r"@KafkaListener",
        r"@JmsListener",
        r"@RabbitListener",
        r"KafkaTemplate",
        r"JmsTemplate",
        r"RabbitTemplate",
    ],
    IntegrationType.SOCKET: [
        r"@ServerEndpoint",
        r"@OnOpen",
        r"@OnMessage",
        r"WebSocketHandler",
        r"ServerSocket",
    ],
    IntegrationType.DATABASE: [
        r"@Repository",
        r"@Entity",
        r"@Query",
        r"@Transactional",
        r"EntityManager",
        r"JdbcTemplate",
        r"PreparedStatement",
    ],
}

# Strong keywords that indicate medium confidence for name matches
STRONG_KEYWORDS = [
    "controller",
    "repository",
    "listener",
    "handler",
    "template",
]


def first_matching_pattern(text: str, patterns: list[str]) -> str | None:
    """Find the first pattern that matches the text.

    Args:
        text: The text to check.
        patterns: List of regex patterns to match against.

    Returns:
        The first matched pattern string, or None if no match.
    """
    return next(
        (pattern for pattern in patterns if re.search(pattern, text)),
        None,
    )


def _is_high_confidence_pattern(
    integration_type: IntegrationType, matched_pattern: str
) -> bool:
    """Check if the matched pattern is a high confidence pattern."""
    high_patterns = HIGH_CONFIDENCE_PATTERNS.get(integration_type, [])
    return any(re.search(hp, matched_pattern) for hp in high_patterns)


def _has_strong_keyword(matched_pattern: str) -> bool:
    """Check if the pattern contains a strong keyword."""
    pattern_lower = matched_pattern.lower()
    return any(kw in pattern_lower for kw in STRONG_KEYWORDS)


def determine_confidence(
    integration_type: IntegrationType,
    matched_pattern: str,
    is_name_match: bool,
    is_signature_match: bool,
    is_scope_match: bool,
) -> Confidence:
    """Determine confidence level based on match characteristics.

    Args:
        integration_type: The type of integration detected.
        matched_pattern: The pattern that triggered the match.
        is_name_match: Whether the match was on the symbol name.
        is_signature_match: Whether the match was on the signature.
        is_scope_match: Whether the match was on the scope.

    Returns:
        Confidence level.
    """
    if _is_high_confidence_pattern(integration_type, matched_pattern):
        return Confidence.HIGH

    if is_signature_match:
        return Confidence.HIGH

    if is_scope_match:
        return Confidence.MEDIUM

    if is_name_match and _has_strong_keyword(matched_pattern):
        return Confidence.MEDIUM

    return Confidence.LOW


def _check_field(
    text: str,
    patterns: list[str],
    integration_type: IntegrationType,
    field_name: str,
    is_name: bool,
    is_signature: bool,
    is_scope: bool,
) -> Iterator[tuple[IntegrationType, Confidence, str]]:
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
    integration_type: IntegrationType,
    patterns: dict[str, list[str]],
) -> Iterator[tuple[IntegrationType, Confidence, str]]:
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
        is_signature=False,
        is_scope=False,
    )
    if entry.signature is not None:
        yield from _check_field(
            entry.signature,
            patterns["signature_patterns"],
            integration_type,
            "signature",
            is_name=False,
            is_signature=True,
            is_scope=False,
        )
    if entry.scope is not None:
        yield from _check_field(
            entry.scope,
            patterns["scope_patterns"],
            integration_type,
            "scope",
            is_name=False,
            is_signature=False,
            is_scope=True,
        )


def classify_entry(entry: CTagsEntry) -> list[tuple[IntegrationType, Confidence, str]]:
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
