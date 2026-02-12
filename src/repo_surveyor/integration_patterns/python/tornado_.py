"""Tornado framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Tornado",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"import tornado", Confidence.HIGH),
                (r"from tornado\.web import", Confidence.HIGH),
                (r"from tornado\.httpclient import", Confidence.HIGH),
                (r"RequestHandler", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"from tornado\.websocket import", Confidence.HIGH),
                (r"WebSocketHandler", Confidence.HIGH),
            ],
        },
    },
)
