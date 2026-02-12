"""Carter framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Carter"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"CarterModule", Confidence.HIGH),
            (r"ICarterModule", Confidence.HIGH),
        ],
    },
}
