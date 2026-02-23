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
                        "HTTP library is imported for server client operations",
                        "HTTP requests are handled for inbound processing",
                    ),
                ),
                (
                    r"HttpRoutes\.of\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP routes are defined with route handlers",
                        "HTTP routes are handled for inbound requests",
                    ),
                ),
                (
                    r"BlazeServerBuilder\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is built via BlazeServerBuilder",
                        "HTTP server is exposed using Blaze",
                    ),
                ),
                (
                    r"EmberServerBuilder\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is built via EmberServerBuilder",
                        "HTTP server is exposed using Ember",
                    ),
                ),
                (
                    r"Ok\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is created with OK status",
                        "HTTP requests are handled with responses",
                    ),
                ),
                (
                    r"case\s+GET\s*->",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is matched for HTTP request handling",
                        "HTTP GET requests are handled for inbound processing",
                    ),
                ),
                (
                    r"case\s+POST\s*->",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is matched for HTTP request handling",
                        "HTTP POST requests are handled for inbound processing",
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
                        "WebSocket support is imported for server implementation",
                        "WebSocket connections are handled for inbound requests",
                    ),
                ),
                (
                    r"WebSocketBuilder",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connection is built for bidirectional communication",
                        "WebSocket connections are accepted for inbound requests",
                    ),
                ),
            ],
        },
    },
)
