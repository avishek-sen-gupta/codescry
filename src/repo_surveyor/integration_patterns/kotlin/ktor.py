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
                        "HTTP routing is imported",
                        "HTTP routing is handled via Ktor",
                    ),
                ),
                (
                    r"routing\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP routes are defined with routing",
                        "HTTP routes are handled via Ktor",
                    ),
                ),
                (
                    r"get\s*\(\"",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "GET request is handled by route",
                        "HTTP GET requests are handled via Ktor",
                    ),
                ),
                (
                    r"post\s*\(\"",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "POST request is handled by route",
                        "HTTP POST requests are handled via Ktor",
                    ),
                ),
                (
                    r"call\.respond\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is sent",
                        "HTTP calls are handled and responded",
                    ),
                ),
                (
                    r"call\.receive\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request body is received",
                        "HTTP request payloads are received inbound",
                    ),
                ),
                (
                    r"embeddedServer\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is created with embeddedServer",
                        "HTTP server is exposed for inbound requests",
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
                        "WebSocket support is imported",
                        "WebSocket connections are handled via Ktor",
                    ),
                ),
                (
                    r"webSocket\s*\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connection is handled by route",
                        "WebSocket connections are accepted via Ktor",
                    ),
                ),
                (
                    r"incoming\.receive\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket frame is received from incoming",
                        "WebSocket frames are received inbound",
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
                        "Server-sent events are imported",
                        "Server-sent event streams are exposed inbound",
                    ),
                ),
                (
                    r"sse\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "SSE streaming is handled by route",
                        "SSE stream connections are handled inbound",
                    ),
                ),
            ],
        },
    },
)
