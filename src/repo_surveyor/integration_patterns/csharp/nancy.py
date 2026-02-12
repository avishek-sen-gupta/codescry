"""Nancy framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Nancy"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"NancyModule", Confidence.HIGH),
            (r"Get\[", Confidence.HIGH),
            (r"Post\[", Confidence.HIGH),
        ],
    },
}
