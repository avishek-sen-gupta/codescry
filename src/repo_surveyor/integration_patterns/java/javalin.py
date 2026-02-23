"""Javalin framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Javalin",
    import_patterns=(r"import io\.javalin",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.javalin",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Javalin",
                        "HTTP endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"Javalin\.create",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is instantiated with Javalin",
                        "REST API is exposed for inbound requests",
                    ),
                ),
                (
                    r"\w*\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is registered for HTTP requests",
                        "HTTP GET requests are handled for inbound requests",
                    ),
                ),
                (
                    r"\w*\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is registered for HTTP requests",
                        "HTTP POST requests are handled for inbound requests",
                    ),
                ),
                (
                    r"\w*\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT route is registered for HTTP requests",
                        "HTTP PUT requests are handled for inbound requests",
                    ),
                ),
                (
                    r"\w*\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE route is registered for HTTP requests",
                        "HTTP DELETE requests are handled for inbound requests",
                    ),
                ),
                (
                    r"\w*\.patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PATCH route is registered for HTTP requests",
                        "HTTP PATCH requests are handled for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"\w*\.ws\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket route is registered for connections",
                        "WebSocket connection is exposed for inbound requests",
                    ),
                ),
                (
                    r"WsConfig",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket handlers are configured with callbacks",
                        "WebSocket connections are handled for inbound requests",
                    ),
                ),
            ],
        },
    },
)
