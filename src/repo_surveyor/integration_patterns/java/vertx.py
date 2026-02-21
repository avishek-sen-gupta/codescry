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
                        "Vert.x import for building Vert.x HTTP server or client components",
                        "This code uses Vert.x to expose an inbound HTTP endpoint",
                    ),
                ),
                (
                    r"vertx\.createHttpServer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Vert.x vertx.createHttpServer call for starting an HTTP server",
                        "This code uses Vert.x to expose an inbound REST API",
                    ),
                ),
                (
                    r"Router\.router",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Vert.x Router.router call for creating an HTTP request router",
                        "This code uses Vert.x to handle inbound HTTP requests",
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
                        "Vert.x ServerWebSocket for handling server-side WebSocket connections",
                        "This code uses Vert.x to expose an inbound WebSocket connection",
                    ),
                ),
                (
                    r"SockJSHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Vert.x SockJSHandler for handling SockJS WebSocket-like connections",
                        "This code uses Vert.x to expose an inbound WebSocket connection",
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
                        "Vert.x EventBus for publishing and consuming messages across verticles",
                        "This code uses Vert.x EventBus to interact with a message queue",
                    ),
                ),
                (
                    r"vertx\.eventBus\(\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Vert.x vertx.eventBus() accessor for obtaining the Vert.x EventBus",
                        "This code uses Vert.x EventBus to interact with a message queue",
                    ),
                ),
            ],
        },
    },
)
