"""Fiber framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Fiber"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r'"github\.com/gofiber/fiber"', Confidence.HIGH),
            (r"fiber\.Ctx", Confidence.HIGH),
            (r"fiber\.New\(", Confidence.HIGH),
        ],
    },
}
