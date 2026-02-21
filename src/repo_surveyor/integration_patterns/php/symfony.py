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
                        "Symfony #[Route] attribute defining an inbound HTTP route",
                        "This code uses Symfony routing to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"extends\s+AbstractController",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Symfony AbstractController subclass handling inbound HTTP requests",
                        "This code uses Symfony to handle inbound HTTP requests via a controller",
                    ),
                ),
                (
                    r"@Route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Symfony @Route annotation defining an inbound HTTP route",
                        "This code uses Symfony routing to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"JsonResponse",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Symfony JsonResponse sending a JSON response to an inbound HTTP request",
                        "This code uses Symfony to expose inbound HTTP endpoints returning JSON responses",
                    ),
                ),
                (
                    r"Request::create\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Symfony Request::create constructing an HTTP request object",
                        "This code uses Symfony Request to interact with HTTP request construction",
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
                        "Symfony Doctrine #[ORM\\Entity] attribute marking a class as a database entity",
                        "This code uses Symfony Doctrine ORM to interact with an outbound database entity",
                    ),
                ),
                (
                    r"#\[ORM\\Table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Symfony Doctrine #[ORM\\Table] attribute mapping a class to a database table",
                        "This code uses Symfony Doctrine ORM to write to an outbound database table",
                    ),
                ),
                (
                    r"@ORM\\Entity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Symfony Doctrine @ORM\\Entity annotation marking a class as a database entity",
                        "This code uses Symfony Doctrine ORM to interact with an outbound database entity",
                    ),
                ),
                (
                    r"EntityManagerInterface",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Symfony Doctrine EntityManagerInterface managing outbound database persistence",
                        "This code uses Symfony Doctrine to write to or query an outbound database",
                    ),
                ),
                (
                    r"EntityRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Symfony Doctrine EntityRepository querying the outbound database for entities",
                        "This code uses Symfony Doctrine to query an outbound database entity repository",
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
                        "Symfony Messenger MessageBusInterface dispatching or handling messages",
                        "This code uses Symfony Messenger to interact with a message bus",
                    ),
                ),
                (
                    r"#\[AsMessageHandler\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Symfony Messenger #[AsMessageHandler] attribute receiving inbound messages",
                        "This code uses Symfony Messenger to handle inbound messages from a message bus",
                    ),
                ),
                (
                    r"Messenger\\Stamp",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Symfony Messenger\\Stamp decorating a message envelope",
                        "This code uses Symfony Messenger to interact with message envelope stamps",
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
                        "Symfony MailerInterface sending outbound email messages",
                        "This code uses Symfony Mailer to send outbound email messages",
                    ),
                ),
                (
                    r"new\s+Email\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Symfony Email object composing an outbound email message",
                        "This code uses Symfony Mailer to compose an outbound email message",
                    ),
                ),
                (
                    r"TemplatedEmail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Symfony TemplatedEmail composing a template-based outbound email",
                        "This code uses Symfony Mailer to send a templated outbound email message",
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
                        "Symfony CacheInterface accessing the configured outbound cache pool",
                        "This code uses Symfony Cache to write to or read from an outbound cache pool",
                    ),
                ),
                (
                    r"TagAwareCacheInterface",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Symfony TagAwareCacheInterface accessing a tag-aware outbound cache pool",
                        "This code uses Symfony Cache to write to or read from an outbound tag-aware cache pool",
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
                        "Symfony Scheduler #[AsCronTask] attribute defining a cron-scheduled task",
                        "This code uses Symfony Scheduler to interact with a cron-expression scheduled task",
                    ),
                ),
                (
                    r"#\[AsPeriodicTask\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Symfony Scheduler #[AsPeriodicTask] attribute defining a periodic scheduled task",
                        "This code uses Symfony Scheduler to interact with a periodic scheduled task",
                    ),
                ),
            ],
        },
    },
)
