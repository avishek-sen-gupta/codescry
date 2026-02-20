"""http4s framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="http4s",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import org\.http4s", Confidence.HIGH, SignalDirection.INWARD),
                (r"HttpRoutes\.of\[", Confidence.HIGH, SignalDirection.INWARD),
                (r"BlazeServerBuilder\[", Confidence.HIGH, SignalDirection.INWARD),
                (r"EmberServerBuilder\[", Confidence.HIGH, SignalDirection.INWARD),
                (r"Ok\(", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"case\s+GET\s*->", Confidence.HIGH, SignalDirection.INWARD),
                (r"case\s+POST\s*->", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.http4s\.server\.websocket",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"WebSocketBuilder", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
