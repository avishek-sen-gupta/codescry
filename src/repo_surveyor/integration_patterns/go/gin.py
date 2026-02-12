"""Gin framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Gin"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r'"github\.com/gin-gonic/gin"', Confidence.HIGH),
            (r"gin\.Context", Confidence.HIGH),
            (r"gin\.Default\(", Confidence.HIGH),
            (r"gin\.New\(", Confidence.HIGH),
        ],
    },
}
