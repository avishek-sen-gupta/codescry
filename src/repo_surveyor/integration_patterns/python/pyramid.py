"""Pyramid framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Pyramid",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from pyramid import", Confidence.HIGH),
                (r"from pyramid\.view import", Confidence.HIGH),
                (r"@view_config", Confidence.HIGH),
                (r"from pyramid\.config import", Confidence.HIGH),
            ],
        },
    },
)
