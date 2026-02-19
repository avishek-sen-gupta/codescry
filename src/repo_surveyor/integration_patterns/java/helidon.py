"""Helidon framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Helidon",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.helidon", Confidence.HIGH, SignalDirection.INWARD),
                (r"WebServer\.builder", Confidence.HIGH, SignalDirection.INWARD),
                (r"HttpRules", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
