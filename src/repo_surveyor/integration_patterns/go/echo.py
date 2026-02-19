"""Echo framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Echo",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/labstack/echo"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"echo\.Context", Confidence.HIGH, SignalDirection.INWARD),
                (r"echo\.New\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
