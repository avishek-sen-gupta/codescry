"""Tornado framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Tornado",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import tornado", Confidence.HIGH),
                (r"from tornado\.web import", Confidence.HIGH),
                (r"from tornado\.httpclient import", Confidence.HIGH),
                (r"RequestHandler", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"from tornado\.websocket import", Confidence.HIGH),
                (r"WebSocketHandler", Confidence.HIGH),
            ],
        },
    },
)
