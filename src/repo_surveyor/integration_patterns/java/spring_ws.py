"""Spring WS framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Spring WS",
    patterns={
        IntegrationType.SOAP: {
            "patterns": [
                (r"@Endpoint", Confidence.HIGH),
                (r"@PayloadRoot", Confidence.HIGH),
                (r"WebServiceTemplate", Confidence.HIGH),
                (r"org\.springframework\.ws", Confidence.HIGH),
            ],
        },
    },
)
