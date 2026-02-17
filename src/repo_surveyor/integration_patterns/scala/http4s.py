"""http4s framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="http4s",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import org\.http4s", Confidence.HIGH),
                (r"HttpRoutes\.of\[", Confidence.HIGH),
                (r"BlazeServerBuilder\[", Confidence.HIGH),
                (r"EmberServerBuilder\[", Confidence.HIGH),
                (r"Ok\(", Confidence.MEDIUM),
                (r"case\s+GET\s*->", Confidence.HIGH),
                (r"case\s+POST\s*->", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"import org\.http4s\.server\.websocket", Confidence.HIGH),
                (r"WebSocketBuilder", Confidence.HIGH),
            ],
        },
    },
)
