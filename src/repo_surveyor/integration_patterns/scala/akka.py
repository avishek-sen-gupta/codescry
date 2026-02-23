"""Akka/Pekko framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Akka HTTP",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import akka\.http\.scaladsl",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported with scaladsl",
                        "HTTP requests are handled by Akka HTTP",
                    ),
                ),
                (
                    r"import org\.apache\.pekko\.http\.scaladsl",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported with Pekko scaladsl",
                        "HTTP requests are handled by Akka Pekko",
                    ),
                ),
                (
                    r"pathPrefix\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "URL path is matched with prefix directive",
                        "HTTP route path prefix is handled for inbound requests",
                    ),
                ),
                (
                    r"complete\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is completed with directive",
                        "HTTP requests are handled with responses by Akka",
                    ),
                ),
                (
                    r"Http\(\)\.newServerAt\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is bound with address",
                        "HTTP server is exposed by Akka HTTP",
                    ),
                ),
                (
                    r"path\(\"",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "URL path is matched with exact directive",
                        "HTTP requests are handled at path by Akka",
                    ),
                ),
                (
                    r"get\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "GET request is handled with directive",
                        "HTTP GET requests are handled by Akka",
                    ),
                ),
                (
                    r"post\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "POST request is handled with directive",
                        "HTTP POST requests are handled by Akka",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"ActorSystem\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Actor system is instantiated for messaging",
                        "Actor messaging system is accessed",
                    ),
                ),
                (
                    r"import akka\.actor",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Actor messaging is imported",
                        "Actor messaging system is accessed",
                    ),
                ),
                (
                    r"import org\.apache\.pekko\.actor",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Actor messaging is imported with Pekko",
                        "Actor messaging system is integrated with Akka",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"handleWebSocketMessages\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connection is handled with messages",
                        "WebSocket connection is handled for inbound communication",
                    ),
                ),
                (
                    r"import akka\.http\.scaladsl\.model\.ws",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket support is imported with model",
                        "WebSocket message is handled for inbound communication",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"import akka\.http\.scaladsl\.model\.sse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-sent events are imported with SSE",
                        "Server-sent event streams are exposed by Akka",
                    ),
                ),
                (
                    r"completeWithSource\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Streaming response is completed with source",
                        "SSE stream is handled for inbound connections",
                    ),
                ),
            ],
        },
    },
)
