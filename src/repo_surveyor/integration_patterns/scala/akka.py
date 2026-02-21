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
                        "Akka HTTP scaladsl import for HTTP server/client",
                        "This code uses Akka HTTP to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"import org\.apache\.pekko\.http\.scaladsl",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP Pekko scaladsl import for HTTP server/client",
                        "This code uses Akka HTTP Pekko to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"pathPrefix\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP pathPrefix directive for URL path matching",
                        "This code uses Akka HTTP to handle inbound HTTP route path prefixes",
                    ),
                ),
                (
                    r"complete\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP complete directive for HTTP response completion",
                        "This code uses Akka HTTP to handle inbound HTTP requests and respond",
                    ),
                ),
                (
                    r"Http\(\)\.newServerAt\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP Http().newServerAt for HTTP server binding",
                        "This code uses Akka HTTP to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"path\(\"",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP path directive for exact URL path matching",
                        "This code uses Akka HTTP to handle inbound HTTP requests at a path",
                    ),
                ),
                (
                    r"get\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP get directive for HTTP GET request handling",
                        "This code uses Akka HTTP to handle inbound HTTP GET requests",
                    ),
                ),
                (
                    r"post\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP post directive for HTTP POST request handling",
                        "This code uses Akka HTTP to handle inbound HTTP POST requests",
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
                        "Akka HTTP ActorSystem instantiation for actor-based messaging",
                        "This code uses Akka HTTP to interact with an actor messaging system",
                    ),
                ),
                (
                    r"import akka\.actor",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Akka HTTP actor import for actor-based messaging",
                        "This code uses Akka HTTP to interact with the actor messaging system",
                    ),
                ),
                (
                    r"import org\.apache\.pekko\.actor",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Akka HTTP Pekko actor import for actor-based messaging",
                        "This code uses Akka HTTP Pekko to interact with the actor messaging system",
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
                        "Akka HTTP handleWebSocketMessages for WebSocket connection handling",
                        "This code uses Akka HTTP to handle inbound WebSocket connections",
                    ),
                ),
                (
                    r"import akka\.http\.scaladsl\.model\.ws",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP WebSocket model import for WebSocket support",
                        "This code uses Akka HTTP to handle inbound WebSocket messages",
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
                        "Akka HTTP SSE model import for server-sent events support",
                        "This code uses Akka HTTP to expose inbound server-sent event streams",
                    ),
                ),
                (
                    r"completeWithSource\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Akka HTTP completeWithSource for streaming response",
                        "This code uses Akka HTTP to handle inbound streaming SSE connections",
                    ),
                ),
            ],
        },
    },
)
