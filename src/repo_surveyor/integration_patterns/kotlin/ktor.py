"""Ktor framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Ktor",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.ktor\.server\.routing",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor server routing import for HTTP route handling",
                        "This code uses Ktor to handle inbound HTTP routing",
                    ),
                ),
                (
                    r"routing\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Ktor routing block for HTTP route definition",
                        "This code uses Ktor to handle inbound HTTP routes",
                    ),
                ),
                (
                    r"get\s*\(\"",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Ktor GET route handler for HTTP GET requests",
                        "This code uses Ktor to handle inbound HTTP GET requests",
                    ),
                ),
                (
                    r"post\s*\(\"",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Ktor POST route handler for HTTP POST requests",
                        "This code uses Ktor to handle inbound HTTP POST requests",
                    ),
                ),
                (
                    r"call\.respond\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor call.respond for HTTP response sending",
                        "This code uses Ktor to handle inbound HTTP calls and respond",
                    ),
                ),
                (
                    r"call\.receive\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor call.receive for HTTP request body reading",
                        "This code uses Ktor to receive inbound HTTP request payloads",
                    ),
                ),
                (
                    r"embeddedServer\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor embeddedServer for HTTP server instantiation",
                        "This code uses Ktor to expose an inbound embedded HTTP server",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.ktor\.server\.websocket",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor server WebSocket import for WebSocket support",
                        "This code uses Ktor to handle inbound WebSocket connections",
                    ),
                ),
                (
                    r"webSocket\s*\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor webSocket route handler for WebSocket connections",
                        "This code uses Ktor to accept inbound WebSocket connections",
                    ),
                ),
                (
                    r"incoming\.receive\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor WebSocket incoming.receive for frame reception",
                        "This code uses Ktor to receive inbound WebSocket frames",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.ktor\.server\.sse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ktor server SSE import for server-sent events support",
                        "This code uses Ktor to expose inbound server-sent event streams",
                    ),
                ),
                (
                    r"sse\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Ktor SSE route handler for server-sent event streaming",
                        "This code uses Ktor to handle inbound SSE stream connections",
                    ),
                ),
            ],
        },
    },
)
