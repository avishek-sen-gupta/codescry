"""Javalin framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Javalin",
    import_patterns=(r"import io\.javalin",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.javalin", Confidence.HIGH),
                (r"Javalin\.create", Confidence.HIGH),
                (r"\w*\.get\(", Confidence.HIGH),
                (r"\w*\.post\(", Confidence.HIGH),
                (r"\w*\.put\(", Confidence.HIGH),
                (r"\w*\.delete\(", Confidence.HIGH),
                (r"\w*\.patch\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\w*\.ws\(", Confidence.HIGH),
                (r"WsConfig", Confidence.HIGH),
            ],
        },
    },
)
