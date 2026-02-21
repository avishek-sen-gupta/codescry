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
                (
                    r"import org\.http4s",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s library import for HTTP server/client",
                        "This code uses http4s to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"HttpRoutes\.of\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s HttpRoutes.of for HTTP route definition",
                        "This code uses http4s to handle inbound HTTP routes",
                    ),
                ),
                (
                    r"BlazeServerBuilder\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s BlazeServerBuilder for HTTP server construction",
                        "This code uses http4s to expose an inbound Blaze HTTP server",
                    ),
                ),
                (
                    r"EmberServerBuilder\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s EmberServerBuilder for HTTP server construction",
                        "This code uses http4s to expose an inbound Ember HTTP server",
                    ),
                ),
                (
                    r"Ok\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "http4s Ok response for HTTP 200 reply",
                        "This code uses http4s to handle inbound HTTP requests and respond",
                    ),
                ),
                (
                    r"case\s+GET\s*->",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s GET route pattern match for HTTP GET handling",
                        "This code uses http4s to handle inbound HTTP GET requests",
                    ),
                ),
                (
                    r"case\s+POST\s*->",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s POST route pattern match for HTTP POST handling",
                        "This code uses http4s to handle inbound HTTP POST requests",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.http4s\.server\.websocket",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s server WebSocket import for WebSocket support",
                        "This code uses http4s to handle inbound WebSocket connections",
                    ),
                ),
                (
                    r"WebSocketBuilder",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "http4s WebSocketBuilder for WebSocket connection handling",
                        "This code uses http4s to accept inbound WebSocket connections",
                    ),
                ),
            ],
        },
    },
)
