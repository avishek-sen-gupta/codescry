"""Drogon framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Drogon",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\bHttpController\b", Confidence.HIGH),
                (r"\bHttpSimpleController\b", Confidence.HIGH),
                (r"\bHttpApiController\b", Confidence.HIGH),
                (r"\bHttpResponse\b", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\bWebSocketController\b", Confidence.HIGH),
            ],
        },
    },
)
