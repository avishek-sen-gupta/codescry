"""Fiber framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Fiber",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'"github\.com/gofiber/fiber"', Confidence.HIGH),
                (r"fiber\.Ctx", Confidence.HIGH),
                (r"fiber\.New\(", Confidence.HIGH),
            ],
        },
    },
)
