"""Hapi framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Hapi",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]@hapi/hapi['\"]\)", Confidence.HIGH),
                (r"Hapi\.server\(", Confidence.HIGH),
                (r"server\.route\(", Confidence.HIGH),
            ],
        },
    },
)
