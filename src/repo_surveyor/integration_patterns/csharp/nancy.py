"""Nancy framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Nancy",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"NancyModule", Confidence.HIGH),
                (r"Get\[", Confidence.HIGH),
                (r"Post\[", Confidence.HIGH),
            ],
        },
    },
)
