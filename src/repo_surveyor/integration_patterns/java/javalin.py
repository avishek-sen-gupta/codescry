"""Javalin framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Javalin",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"import io\.javalin", Confidence.HIGH),
                (r"Javalin\.create", Confidence.HIGH),
                (r"\w+\.get\(", Confidence.HIGH),
                (r"\w+\.post\(", Confidence.HIGH),
                (r"\w+\.put\(", Confidence.HIGH),
                (r"\w+\.delete\(", Confidence.HIGH),
                (r"\w+\.patch\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"\w+\.ws\(", Confidence.HIGH),
                (r"WsConfig", Confidence.HIGH),
            ],
        },
    },
)
