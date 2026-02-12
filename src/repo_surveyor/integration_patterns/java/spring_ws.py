"""Spring WS framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Spring WS"

PATTERNS = {
    IntegrationType.SOAP: {
        "patterns": [
            (r"@Endpoint", Confidence.HIGH),
            (r"@PayloadRoot", Confidence.HIGH),
            (r"WebServiceTemplate", Confidence.HIGH),
            (r"org\.springframework\.ws", Confidence.HIGH),
        ],
    },
}
