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
                (r"import io\.javalin", Confidence.HIGH, SignalDirection.INWARD),
                (r"Javalin\.create", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.delete\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\w*\.patch\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\w*\.ws\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"WsConfig", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
