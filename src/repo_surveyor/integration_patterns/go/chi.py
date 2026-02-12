"""Chi framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Chi",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/go-chi/chi"', Confidence.HIGH),
                (r"chi\.NewRouter\(", Confidence.HIGH),
            ],
        },
    },
)
