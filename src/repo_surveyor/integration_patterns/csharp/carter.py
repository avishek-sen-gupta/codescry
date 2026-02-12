"""Carter framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Carter",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"CarterModule", Confidence.HIGH),
                (r"ICarterModule", Confidence.HIGH),
            ],
        },
    },
)
