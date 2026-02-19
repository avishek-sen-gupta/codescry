"""Gin framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Gin",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/gin-gonic/gin"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"gin\.Context", Confidence.HIGH, SignalDirection.INWARD),
                (r"gin\.Default\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"gin\.New\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
