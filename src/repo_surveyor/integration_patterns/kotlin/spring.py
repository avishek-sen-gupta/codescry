"""Spring framework integration patterns for Kotlin."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Spring",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"@RestController",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "REST endpoint is annotated for controller",
                        "REST API endpoints are exposed for inbound requests",
                    ),
                ),
                (
                    r"@GetMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP GET is handled by mapping",
                        "HTTP GET requests are handled for inbound requests",
                    ),
                ),
                (
                    r"@PostMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP POST is handled by mapping",
                        "HTTP POST requests are handled for inbound requests",
                    ),
                ),
                (
                    r"@PutMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP PUT is handled by mapping",
                        "HTTP PUT requests are handled for inbound requests",
                    ),
                ),
                (
                    r"@DeleteMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP DELETE is handled by mapping",
                        "HTTP DELETE requests are handled for inbound requests",
                    ),
                ),
                (
                    r"@RequestMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route is mapped by annotation",
                        "HTTP requests are handled for inbound requests",
                    ),
                ),
                (
                    r"coRouter\s*\{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Coroutine routing is defined with coRouter block",
                        "HTTP routes are handled with coroutines for inbound requests",
                    ),
                ),
                (
                    r"router\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP routing is defined with router block",
                        "HTTP routes are handled functionally for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"@KafkaListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kafka message is consumed by listener",
                        "Kafka messages are consumed from inbound topic",
                    ),
                ),
                (
                    r"@JmsListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JMS message is consumed by listener",
                        "JMS messages are received from inbound queue",
                    ),
                ),
                (
                    r"@RabbitListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "RabbitMQ message is consumed by listener",
                        "RabbitMQ messages are received from inbound queue",
                    ),
                ),
                (
                    r"KafkaTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kafka message is published via template",
                        "Kafka messages are produced for outbound delivery",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"@Repository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database repository is annotated for access",
                        "Database is queried via repository pattern",
                    ),
                ),
                (
                    r"@Query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database query is annotated as custom",
                        "Database is queried with custom statement",
                    ),
                ),
                (
                    r"JdbcTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are handled by JDBC template",
                        "Database is queried via JDBC connection",
                    ),
                ),
                (
                    r"R2dbcRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Reactive database is accessed by repository",
                        "Database is queried reactively via R2DBC",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"@Cacheable",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache pattern is annotated as cacheable",
                        "Cache layer is accessed",
                    ),
                ),
                (
                    r"@CacheEvict",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache invalidation is annotated for eviction",
                        "Cache is evicted using Spring framework",
                    ),
                ),
                (
                    r"@EnableCaching",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache configuration is enabled by annotation",
                        "Cache layer is integrated",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"@Scheduled",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Task execution is scheduled by annotation",
                        "Scheduled tasks are executed using Spring framework",
                    ),
                ),
                (
                    r"@EnableScheduling",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduling configuration is enabled by annotation",
                        "Task scheduling is managed using Spring framework",
                    ),
                ),
            ],
        },
    },
)
