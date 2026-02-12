"""ServiceStack framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="ServiceStack",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"using ServiceStack", Confidence.HIGH),
                (r"IReturn<", Confidence.HIGH),
                (r"IGet\b", Confidence.HIGH),
                (r"IPost\b", Confidence.HIGH),
            ],
        },
    },
)
