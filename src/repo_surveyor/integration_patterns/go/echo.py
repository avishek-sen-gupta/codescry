"""Echo framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Echo",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'"github\.com/labstack/echo"', Confidence.HIGH),
                (r"echo\.Context", Confidence.HIGH),
                (r"echo\.New\(", Confidence.HIGH),
            ],
        },
    },
)
