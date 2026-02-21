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
                        "Drogon HttpController class defining an inbound HTTP controller",
                        "This code uses Drogon to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"\bHttpSimpleController\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Drogon HttpSimpleController class for simple inbound HTTP handlers",
                        "This code uses Drogon to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"\bHttpApiController\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Drogon HttpApiController class for inbound REST API handlers",
                        "This code uses Drogon to expose an inbound REST API",
                    ),
                ),
                (
                    r"\bHttpResponse\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Drogon HttpResponse class constructing an HTTP response",
                        "This code uses Drogon to handle inbound HTTP requests",
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
                        "Drogon WebSocketController class for inbound WebSocket connections",
                        "This code uses Drogon to accept inbound WebSocket connections",
                    ),
                ),
            ],
        },
    },
)
