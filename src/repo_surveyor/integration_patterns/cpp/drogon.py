"""Drogon framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Drogon",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"\bHttpController\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP controller is defined for inbound requests",
                        "HTTP requests are handled inbound",
                    ),
                ),
                (
                    r"\bHttpSimpleController\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Simple HTTP handler is defined for inbound requests",
                        "HTTP requests are handled inbound",
                    ),
                ),
                (
                    r"\bHttpApiController\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "REST API handler is defined by HttpApiController",
                        "REST API is exposed inbound",
                    ),
                ),
                (
                    r"\bHttpResponse\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is constructed by HttpResponse",
                        "HTTP requests are handled inbound",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"\bWebSocketController\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connection is handled for inbound requests",
                        "WebSocket connections are accepted inbound",
                    ),
                ),
            ],
        },
    },
)
