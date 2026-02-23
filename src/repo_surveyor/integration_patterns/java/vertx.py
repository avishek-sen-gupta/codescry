"""Vert.x framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Vert.x",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.vertx",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP components are imported via Vert.x",
                        "HTTP endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"vertx\.createHttpServer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is created with configuration",
                        "REST API is exposed for inbound requests",
                    ),
                ),
                (
                    r"Router\.router",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP router is created for requests",
                        "HTTP request is handled by server",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"ServerWebSocket",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connection is handled server-side",
                        "WebSocket connection is exposed for inbound events",
                    ),
                ),
                (
                    r"SockJSHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SockJS connection is handled by handler",
                        "WebSocket connection is exposed for inbound events",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"EventBus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Messages are published via EventBus",
                        "Message queue is accessed via event bus",
                    ),
                ),
                (
                    r"vertx\.eventBus\(\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "EventBus is accessed for messaging",
                        "Message queue is accessed via event bus",
                    ),
                ),
            ],
        },
    },
)
