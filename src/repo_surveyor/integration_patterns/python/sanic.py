"""Sanic framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Sanic",
    import_patterns=(r"from sanic import", r"import sanic"),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from sanic import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Sanic\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@\w+\.get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@\w+\.post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@\w+\.put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@\w+\.delete\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"@\w+\.websocket\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
