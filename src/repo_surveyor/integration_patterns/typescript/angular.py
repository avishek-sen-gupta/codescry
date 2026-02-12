"""Angular framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Angular",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from ['\"]@angular/common/http['\"]", Confidence.HIGH),
                (r"HttpClient", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"from ['\"]@angular/.*websocket", Confidence.MEDIUM),
            ],
        },
    },
)
