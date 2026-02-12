"""Chi framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Chi",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'"github\.com/go-chi/chi"', Confidence.HIGH),
                (r"chi\.NewRouter\(", Confidence.HIGH),
            ],
        },
    },
)
