"""Chi framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Chi",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/go-chi/chi"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"chi\.NewRouter\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
