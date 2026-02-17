"""Symfony framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Symfony",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"#\[Route\(", Confidence.HIGH),
                (r"extends\s+AbstractController", Confidence.HIGH),
                (r"@Route\(", Confidence.HIGH),
                (r"JsonResponse", Confidence.MEDIUM),
                (r"Request::create\(", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"#\[ORM\\Entity", Confidence.HIGH),
                (r"#\[ORM\\Table", Confidence.HIGH),
                (r"@ORM\\Entity", Confidence.HIGH),
                (r"EntityManagerInterface", Confidence.HIGH),
                (r"EntityRepository", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"MessageBusInterface", Confidence.HIGH),
                (r"#\[AsMessageHandler\]", Confidence.HIGH),
                (r"Messenger\\Stamp", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"MailerInterface", Confidence.HIGH),
                (r"new\s+Email\(", Confidence.MEDIUM),
                (r"TemplatedEmail", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"CacheInterface", Confidence.HIGH),
                (r"TagAwareCacheInterface", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"#\[AsCronTask\(", Confidence.HIGH),
                (r"#\[AsPeriodicTask\(", Confidence.HIGH),
            ],
        },
    },
)
