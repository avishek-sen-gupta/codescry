"""Ktor framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Ktor",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.ktor\.server\.routing", Confidence.HIGH),
                (r"routing\s*\{", Confidence.MEDIUM),
                (r"get\s*\(\"", Confidence.MEDIUM),
                (r"post\s*\(\"", Confidence.MEDIUM),
                (r"call\.respond\(", Confidence.HIGH),
                (r"call\.receive\(", Confidence.HIGH),
                (r"embeddedServer\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"import io\.ktor\.server\.websocket", Confidence.HIGH),
                (r"webSocket\s*\(", Confidence.HIGH),
                (r"incoming\.receive\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"import io\.ktor\.server\.sse", Confidence.HIGH),
                (r"sse\s*\(", Confidence.MEDIUM),
            ],
        },
    },
)
