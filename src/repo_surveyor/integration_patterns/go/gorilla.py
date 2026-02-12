"""Gorilla framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Gorilla"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r'"github\.com/gorilla/mux"', Confidence.HIGH),
            (r"mux\.NewRouter\(", Confidence.HIGH),
        ],
    },
}
