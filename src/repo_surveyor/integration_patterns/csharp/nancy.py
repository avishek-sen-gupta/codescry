"""Nancy framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Nancy",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"NancyModule", Confidence.HIGH, SignalDirection.INWARD),
                (r"Get\[", Confidence.HIGH, SignalDirection.INWARD),
                (r"Post\[", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
