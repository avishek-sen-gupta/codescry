"""Echo framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Echo",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/labstack/echo"', Confidence.HIGH),
                (r"echo\.Context", Confidence.HIGH),
                (r"echo\.New\(", Confidence.HIGH),
            ],
        },
    },
)
