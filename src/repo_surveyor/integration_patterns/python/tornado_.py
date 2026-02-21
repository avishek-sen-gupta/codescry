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
                        "Tornado framework import for async web server and HTTP applications",
                        "This code uses Tornado to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"from tornado\.web import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Tornado web module import for request handler and application routing",
                        "This code uses Tornado to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"from tornado\.httpclient import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Tornado httpclient module import for making outbound HTTP requests",
                        "This code uses Tornado to send outbound HTTP API calls",
                    ),
                ),
                (
                    r"RequestHandler",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Tornado RequestHandler class for handling inbound HTTP requests",
                        "This code uses Tornado to handle incoming HTTP requests",
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
                        "Tornado websocket module import for WebSocket server handler",
                        "This code uses Tornado to handle incoming WebSocket connection requests",
                    ),
                ),
                (
                    r"WebSocketHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Tornado WebSocketHandler class for managing inbound WebSocket connections",
                        "This code uses Tornado to handle incoming WebSocket connection requests",
                    ),
                ),
            ],
        },
    },
)
