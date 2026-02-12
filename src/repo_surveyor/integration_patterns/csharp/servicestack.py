"""ServiceStack framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="ServiceStack",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"using ServiceStack", Confidence.HIGH),
                (r"IReturn<", Confidence.HIGH),
                (r"IGet\b", Confidence.HIGH),
                (r"IPost\b", Confidence.HIGH),
            ],
        },
    },
)
