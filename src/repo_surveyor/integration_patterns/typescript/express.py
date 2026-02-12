"""Express framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Express"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"from ['\"]express['\"]", Confidence.HIGH),
            (r"\w+\.get\(", Confidence.HIGH),
            (r"\w+\.post\(", Confidence.HIGH),
            (r"\w+\.put\(", Confidence.HIGH),
            (r"\w+\.delete\(", Confidence.HIGH),
        ],
    },
}
