"""Helidon framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Helidon",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.helidon", Confidence.HIGH),
                (r"WebServer\.builder", Confidence.HIGH),
                (r"HttpRules", Confidence.HIGH),
            ],
        },
    },
)
