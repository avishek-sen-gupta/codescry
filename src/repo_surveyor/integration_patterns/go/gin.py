"""Gin framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Gin",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'"github\.com/gin-gonic/gin"', Confidence.HIGH),
                (r"gin\.Context", Confidence.HIGH),
                (r"gin\.Default\(", Confidence.HIGH),
                (r"gin\.New\(", Confidence.HIGH),
            ],
        },
    },
)
