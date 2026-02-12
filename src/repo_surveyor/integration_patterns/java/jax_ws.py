"""JAX-WS framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="JAX-WS",
    patterns={
        IntegrationType.SOAP: {
            "patterns": [
                (r"javax\.xml\.ws", Confidence.HIGH),
                (r"jakarta\.xml\.ws", Confidence.HIGH),
                (r"@WebServiceClient", Confidence.HIGH),
                (r"Service\.create", Confidence.HIGH),
            ],
        },
    },
)
