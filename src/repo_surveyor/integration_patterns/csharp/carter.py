"""Carter framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Carter",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"CarterModule", Confidence.HIGH),
                (r"ICarterModule", Confidence.HIGH),
            ],
        },
    },
)
