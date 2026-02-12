"""WCF framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "WCF"

PATTERNS = {
    IntegrationType.SOAP: {
        "patterns": [
            (r"System\.ServiceModel", Confidence.HIGH),
            (r"ChannelFactory", Confidence.HIGH),
            (r"BasicHttpBinding", Confidence.HIGH),
            (r"ServiceHost", Confidence.HIGH),
        ],
    },
}
