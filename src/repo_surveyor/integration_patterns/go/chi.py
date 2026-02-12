"""Chi framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Chi"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r'"github\.com/go-chi/chi"', Confidence.HIGH),
            (r"chi\.NewRouter\(", Confidence.HIGH),
        ],
    },
}
