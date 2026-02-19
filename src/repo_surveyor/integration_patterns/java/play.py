"""Play framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Play",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import play\.mvc", Confidence.HIGH, SignalDirection.INWARD),
                (r"import play\.api\.mvc", Confidence.HIGH, SignalDirection.INWARD),
                (r"Action \{", Confidence.HIGH, SignalDirection.INWARD),
                (r"Action\.async", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
