"""WCF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="WCF",
    patterns={
        IntegrationType.SOAP: {
            "patterns": [
                (r"System\.ServiceModel", Confidence.HIGH),
                (r"ChannelFactory", Confidence.HIGH),
                (r"BasicHttpBinding", Confidence.HIGH),
                (r"ServiceHost", Confidence.HIGH),
            ],
        },
    },
)
