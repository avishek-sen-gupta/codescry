"""ServiceStack framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "ServiceStack"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"using ServiceStack", Confidence.HIGH),
            (r"IReturn<", Confidence.HIGH),
            (r"IGet\b", Confidence.HIGH),
            (r"IPost\b", Confidence.HIGH),
        ],
    },
}
