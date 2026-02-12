"""Gorilla framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Gorilla",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'"github\.com/gorilla/mux"', Confidence.HIGH),
                (r"mux\.NewRouter\(", Confidence.HIGH),
            ],
        },
    },
)
