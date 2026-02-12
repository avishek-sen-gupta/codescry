"""CoreWCF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="CoreWCF",
    patterns={
        IntegrationType.SOAP: {
            "patterns": [
                (r"using CoreWCF", Confidence.HIGH),
                (r"CoreWCF\.Http", Confidence.HIGH),
            ],
        },
    },
)
