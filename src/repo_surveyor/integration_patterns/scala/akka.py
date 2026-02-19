"""Akka/Pekko framework integration patterns."""

from ..types import (
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
                ),
                (
                    r"import org\.apache\.pekko\.http\.scaladsl",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"pathPrefix\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"complete\(", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"Http\(\)\.newServerAt\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"path\(\"", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"get\s*\{", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"post\s*\{", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"ActorSystem\(", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"import akka\.actor", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"import org\.apache\.pekko\.actor",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"handleWebSocketMessages\(", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"import akka\.http\.scaladsl\.model\.ws",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"import akka\.http\.scaladsl\.model\.sse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"completeWithSource\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
