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
                (r"#\[Route\(", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"extends\s+AbstractController",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"@Route\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"JsonResponse", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"Request::create\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"#\[ORM\\Entity", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"#\[ORM\\Table", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@ORM\\Entity", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EntityManagerInterface", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EntityRepository", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"MessageBusInterface", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"#\[AsMessageHandler\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"Messenger\\Stamp", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"MailerInterface", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"new\s+Email\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"TemplatedEmail", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"CacheInterface", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TagAwareCacheInterface", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"#\[AsCronTask\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"#\[AsPeriodicTask\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
