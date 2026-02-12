"""Vert.x framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Vert.x"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"import io\.vertx", Confidence.HIGH),
            (r"vertx\.createHttpServer", Confidence.HIGH),
            (r"Router\.router", Confidence.HIGH),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"ServerWebSocket", Confidence.HIGH),
            (r"SockJSHandler", Confidence.HIGH),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"EventBus", Confidence.HIGH),
            (r"vertx\.eventBus\(\)", Confidence.HIGH),
        ],
    },
}
