"""Play framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Play",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import play\.mvc", Confidence.HIGH),
                (r"import play\.api\.mvc", Confidence.HIGH),
                (r"Action \{", Confidence.HIGH),
                (r"Action\.async", Confidence.HIGH),
            ],
        },
    },
)
