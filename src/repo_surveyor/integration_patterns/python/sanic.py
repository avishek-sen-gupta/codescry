"""Sanic framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Sanic",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from sanic import", Confidence.HIGH),
                (r"Sanic\(", Confidence.HIGH),
                (r"@\w+\.get\(", Confidence.HIGH),
                (r"@\w+\.post\(", Confidence.HIGH),
                (r"@\w+\.put\(", Confidence.HIGH),
                (r"@\w+\.delete\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"@\w+\.websocket\(", Confidence.HIGH),
            ],
        },
    },
)
