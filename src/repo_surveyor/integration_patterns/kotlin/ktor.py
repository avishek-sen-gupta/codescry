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
                ),
                (r"routing\s*\{", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"get\s*\(\"", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"post\s*\(\"", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"call\.respond\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"call\.receive\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"embeddedServer\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.ktor\.server\.websocket",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"webSocket\s*\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"incoming\.receive\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.ktor\.server\.sse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"sse\s*\(", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
    },
)
