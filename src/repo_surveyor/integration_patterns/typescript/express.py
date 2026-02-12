"""Express framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Express",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from ['\"]express['\"]", Confidence.HIGH),
                (r"\w+\.get\(", Confidence.HIGH),
                (r"\w+\.post\(", Confidence.HIGH),
                (r"\w+\.put\(", Confidence.HIGH),
                (r"\w+\.delete\(", Confidence.HIGH),
            ],
        },
    },
)
