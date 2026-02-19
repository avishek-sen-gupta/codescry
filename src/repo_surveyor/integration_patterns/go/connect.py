"""Connect framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Connect",
    patterns={
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r'"connectrpc\.com/connect"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"connect\.NewHandler", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"connect\.NewClient", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
    },
)
