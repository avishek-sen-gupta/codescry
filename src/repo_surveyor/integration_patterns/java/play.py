"""Play framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Play"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"import play\.mvc", Confidence.HIGH),
            (r"import play\.api\.mvc", Confidence.HIGH),
            (r"Action \{", Confidence.HIGH),
            (r"Action\.async", Confidence.HIGH),
        ],
    },
}
