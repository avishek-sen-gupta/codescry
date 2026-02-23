"""Symfony framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Symfony",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"#\[Route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route is defined with Route attribute",
                        "HTTP requests are handled by routing",
                    ),
                ),
                (
                    r"extends\s+AbstractController",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP requests are handled by controller subclass",
                        "HTTP requests are handled by controller",
                    ),
                ),
                (
                    r"@Route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route is defined with Route annotation",
                        "HTTP requests are handled by routing",
                    ),
                ),
                (
                    r"JsonResponse",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "JSON response is sent to HTTP request",
                        "JSON responses are returned from endpoints",
                    ),
                ),
                (
                    r"Request::create\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP request object is constructed",
                        "HTTP request is constructed",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"#\[ORM\\Entity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database entity is marked with ORM attribute",
                        "Database entity is accessed via ORM",
                    ),
                ),
                (
                    r"#\[ORM\\Table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is mapped with ORM attribute",
                        "Database table is written via ORM",
                    ),
                ),
                (
                    r"@ORM\\Entity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database entity is marked with ORM annotation",
                        "Database entity is accessed via ORM",
                    ),
                ),
                (
                    r"EntityManagerInterface",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database persistence is managed by EntityManager interface",
                        "Database is accessed via Doctrine",
                    ),
                ),
                (
                    r"EntityRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database entities are queried by repository",
                        "Database entity repository is queried",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"MessageBusInterface",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Messages are dispatched via bus interface",
                        "Message bus is accessed",
                    ),
                ),
                (
                    r"#\[AsMessageHandler\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Messages are received by handler attribute",
                        "Messages are handled from message bus",
                    ),
                ),
                (
                    r"Messenger\\Stamp",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Message envelope is decorated with stamp",
                        "Message envelope stamps are processed",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"MailerInterface",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email messages are sent via mailer interface",
                        "Email messages are sent outbound",
                    ),
                ),
                (
                    r"new\s+Email\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is composed for outbound delivery",
                        "Email message is composed for sending",
                    ),
                ),
                (
                    r"TemplatedEmail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Template email is composed for outbound delivery",
                        "Templated email is sent outbound",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"CacheInterface",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache pool is accessed via interface",
                        "Cache pool is accessed for read/write",
                    ),
                ),
                (
                    r"TagAwareCacheInterface",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Tag-aware cache pool is accessed via interface",
                        "Tag-aware cache pool is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"#\[AsCronTask\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cron task is defined with scheduler attribute",
                        "Cron-scheduled task is executed",
                    ),
                ),
                (
                    r"#\[AsPeriodicTask\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Periodic task is defined with scheduler attribute",
                        "Periodic task is scheduled",
                    ),
                ),
            ],
        },
    },
)
