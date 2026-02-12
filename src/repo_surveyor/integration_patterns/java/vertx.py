"""Vert.x framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Vert.x",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.vertx", Confidence.HIGH),
                (r"vertx\.createHttpServer", Confidence.HIGH),
                (r"Router\.router", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"ServerWebSocket", Confidence.HIGH),
                (r"SockJSHandler", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"EventBus", Confidence.HIGH),
                (r"vertx\.eventBus\(\)", Confidence.HIGH),
            ],
        },
    },
)
