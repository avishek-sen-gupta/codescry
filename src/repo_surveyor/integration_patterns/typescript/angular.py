"""Angular framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Angular",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from ['\"]@angular/common/http['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"HttpClient", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"from ['\"]@angular/.*websocket", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
    },
)
