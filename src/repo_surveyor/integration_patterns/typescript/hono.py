"""Hono framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Hono",
    import_patterns=(r"from ['\"]hono['\"]",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from ['\"]hono['\"]", Confidence.HIGH, SignalDirection.INWARD),
                (r"new Hono\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"app\.get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"app\.post\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
