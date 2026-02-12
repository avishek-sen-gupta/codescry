"""JAX-WS framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "JAX-WS"

PATTERNS = {
    IntegrationType.SOAP: {
        "patterns": [
            (r"javax\.xml\.ws", Confidence.HIGH),
            (r"jakarta\.xml\.ws", Confidence.HIGH),
            (r"@WebServiceClient", Confidence.HIGH),
            (r"Service\.create", Confidence.HIGH),
        ],
    },
}
