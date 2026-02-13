"""Hono framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Hono",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from ['\"]hono['\"]", Confidence.HIGH),
                (r"new Hono\(", Confidence.HIGH),
                (r"app\.get\(", Confidence.HIGH),
                (r"app\.post\(", Confidence.HIGH),
            ],
        },
    },
)
