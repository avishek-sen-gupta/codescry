"""Vert.x framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Vert.x",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.vertx", Confidence.HIGH, SignalDirection.INWARD),
                (r"vertx\.createHttpServer", Confidence.HIGH, SignalDirection.INWARD),
                (r"Router\.router", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"ServerWebSocket", Confidence.HIGH, SignalDirection.INWARD),
                (r"SockJSHandler", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"EventBus", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"vertx\.eventBus\(\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
