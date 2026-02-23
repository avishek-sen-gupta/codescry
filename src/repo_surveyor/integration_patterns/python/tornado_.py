"""Tornado framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Tornado",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import tornado",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Web server is imported for HTTP applications",
                        "HTTP endpoint is accessed",
                    ),
                ),
                (
                    r"from tornado\.web import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Request handlers are imported for routing",
                        "HTTP endpoint is accessed",
                    ),
                ),
                (
                    r"from tornado\.httpclient import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP requests are made via httpclient",
                        "HTTP API calls are sent outbound",
                    ),
                ),
                (
                    r"RequestHandler",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP requests are handled by RequestHandler",
                        "HTTP requests are handled",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"from tornado\.websocket import",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket handlers are imported for server",
                        "WebSocket connections are handled",
                    ),
                ),
                (
                    r"WebSocketHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connections are managed by handler",
                        "WebSocket connections are handled",
                    ),
                ),
            ],
        },
    },
)
