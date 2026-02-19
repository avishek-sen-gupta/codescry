"""Express framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Express",
    import_patterns=(r"from ['\"]express['\"]",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from ['\"]express['\"]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.delete\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
