"""Echo framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Echo"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r'"github\.com/labstack/echo"', Confidence.HIGH),
            (r"echo\.Context", Confidence.HIGH),
            (r"echo\.New\(", Confidence.HIGH),
        ],
    },
}
