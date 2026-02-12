"""Gorilla framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Gorilla",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/gorilla/mux"', Confidence.HIGH),
                (r"mux\.NewRouter\(", Confidence.HIGH),
            ],
        },
    },
)
