"""Tornado framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Tornado",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import tornado", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"from tornado\.web import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"from tornado\.httpclient import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"RequestHandler", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"from tornado\.websocket import",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"WebSocketHandler", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
