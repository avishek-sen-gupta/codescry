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
                        "Kotlin Spring @RestController annotation for REST API endpoint",
                        "This code uses Kotlin Spring to expose inbound REST API endpoints",
                    ),
                ),
                (
                    r"@GetMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring @GetMapping annotation for HTTP GET handler",
                        "This code uses Kotlin Spring to handle inbound HTTP GET requests",
                    ),
                ),
                (
                    r"@PostMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring @PostMapping annotation for HTTP POST handler",
                        "This code uses Kotlin Spring to handle inbound HTTP POST requests",
                    ),
                ),
                (
                    r"@PutMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring @PutMapping annotation for HTTP PUT handler",
                        "This code uses Kotlin Spring to handle inbound HTTP PUT requests",
                    ),
                ),
                (
                    r"@DeleteMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring @DeleteMapping annotation for HTTP DELETE handler",
                        "This code uses Kotlin Spring to handle inbound HTTP DELETE requests",
                    ),
                ),
                (
                    r"@RequestMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring @RequestMapping annotation for HTTP route mapping",
                        "This code uses Kotlin Spring to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"coRouter\s*\{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring coRouter block for coroutine-based routing",
                        "This code uses Kotlin Spring to handle inbound HTTP routes with coroutines",
                    ),
                ),
                (
                    r"router\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring router block for functional HTTP routing",
                        "This code uses Kotlin Spring to handle inbound HTTP routes functionally",
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
                        "Kotlin Spring @KafkaListener annotation for Kafka message consumption",
                        "This code uses Kotlin Spring to receive inbound Kafka messages",
                    ),
                ),
                (
                    r"@JmsListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring @JmsListener annotation for JMS message consumption",
                        "This code uses Kotlin Spring to receive inbound JMS messages",
                    ),
                ),
                (
                    r"@RabbitListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin Spring @RabbitListener annotation for RabbitMQ message consumption",
                        "This code uses Kotlin Spring to receive inbound RabbitMQ messages",
                    ),
                ),
                (
                    r"KafkaTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Spring KafkaTemplate for Kafka message publishing",
                        "This code uses Kotlin Spring to produce outgoing Kafka messages",
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
                        "Kotlin Spring @Repository annotation for database repository",
                        "This code uses Kotlin Spring to query a database via a repository",
                    ),
                ),
                (
                    r"@Query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Spring @Query annotation for custom database query",
                        "This code uses Kotlin Spring to query a database with a custom statement",
                    ),
                ),
                (
                    r"JdbcTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Spring JdbcTemplate for JDBC database operations",
                        "This code uses Kotlin Spring to query a relational database via JDBC",
                    ),
                ),
                (
                    r"R2dbcRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Spring R2dbcRepository for reactive database access",
                        "This code uses Kotlin Spring to query a database reactively via R2DBC",
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
                        "Kotlin Spring @Cacheable annotation for cache-aside pattern",
                        "This code uses Kotlin Spring to interact with a cache layer",
                    ),
                ),
                (
                    r"@CacheEvict",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin Spring @CacheEvict annotation for cache invalidation",
                        "This code uses Kotlin Spring to interact with cache eviction",
                    ),
                ),
                (
                    r"@EnableCaching",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin Spring @EnableCaching annotation for cache configuration",
                        "This code uses Kotlin Spring to integrate with a caching layer",
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
                        "Kotlin Spring @Scheduled annotation for scheduled task execution",
                        "This code uses Kotlin Spring to interact with scheduled tasks",
                    ),
                ),
                (
                    r"@EnableScheduling",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin Spring @EnableScheduling annotation for scheduling configuration",
                        "This code uses Kotlin Spring to interact with the task scheduling system",
                    ),
                ),
            ],
        },
    },
)
