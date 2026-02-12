"""Nancy framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Nancy",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"NancyModule", Confidence.HIGH),
                (r"Get\[", Confidence.HIGH),
                (r"Post\[", Confidence.HIGH),
            ],
        },
    },
)
