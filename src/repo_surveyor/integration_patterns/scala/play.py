"""Play framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Play",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import play\.api\.mvc", Confidence.HIGH),
                (r"Action\s*\{", Confidence.HIGH),
                (r"Action\.async\s*\{", Confidence.HIGH),
                (r"Ok\(", Confidence.MEDIUM),
                (r"Json\.toJson\(", Confidence.HIGH),
                (r"def\s+\w+.*=\s*Action", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import play\.api\.db", Confidence.HIGH),
                (r"import anorm\.", Confidence.HIGH),
                (r"SQL\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"import play\.api\.libs\.streams", Confidence.HIGH),
                (r"WebSocket\.accept", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"import play\.api\.cache", Confidence.HIGH),
                (r"@Cached", Confidence.HIGH),
            ],
        },
    },
)
