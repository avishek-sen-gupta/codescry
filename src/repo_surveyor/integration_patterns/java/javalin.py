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
                        "Javalin import for setting up a Javalin HTTP server",
                        "This code uses Javalin to expose an inbound HTTP endpoint",
                    ),
                ),
                (
                    r"Javalin\.create",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Javalin Javalin.create call for instantiating a Javalin HTTP server",
                        "This code uses Javalin to expose an inbound REST API",
                    ),
                ),
                (
                    r"\w*\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Javalin .get() route registration for handling HTTP GET requests",
                        "This code uses Javalin to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"\w*\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Javalin .post() route registration for handling HTTP POST requests",
                        "This code uses Javalin to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"\w*\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Javalin .put() route registration for handling HTTP PUT requests",
                        "This code uses Javalin to handle incoming HTTP PUT requests",
                    ),
                ),
                (
                    r"\w*\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Javalin .delete() route registration for handling HTTP DELETE requests",
                        "This code uses Javalin to handle incoming HTTP DELETE requests",
                    ),
                ),
                (
                    r"\w*\.patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Javalin .patch() route registration for handling HTTP PATCH requests",
                        "This code uses Javalin to handle incoming HTTP PATCH requests",
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
                        "Javalin .ws() route registration for handling WebSocket connections",
                        "This code uses Javalin to expose an inbound WebSocket connection",
                    ),
                ),
                (
                    r"WsConfig",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Javalin WsConfig for configuring WebSocket handler callbacks",
                        "This code uses Javalin to handle inbound WebSocket connections",
                    ),
                ),
            ],
        },
    },
)
