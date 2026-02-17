"""Akka/Pekko framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Akka HTTP",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import akka\.http\.scaladsl", Confidence.HIGH),
                (r"import org\.apache\.pekko\.http\.scaladsl", Confidence.HIGH),
                (r"pathPrefix\(", Confidence.HIGH),
                (r"complete\(", Confidence.MEDIUM),
                (r"Http\(\)\.newServerAt\(", Confidence.HIGH),
                (r"path\(\"", Confidence.MEDIUM),
                (r"get\s*\{", Confidence.MEDIUM),
                (r"post\s*\{", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"ActorSystem\(", Confidence.MEDIUM),
                (r"import akka\.actor", Confidence.HIGH),
                (r"import org\.apache\.pekko\.actor", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"handleWebSocketMessages\(", Confidence.HIGH),
                (r"import akka\.http\.scaladsl\.model\.ws", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"import akka\.http\.scaladsl\.model\.sse", Confidence.HIGH),
                (r"completeWithSource\(", Confidence.HIGH),
            ],
        },
    },
)
