"""CoreWCF framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "CoreWCF"

PATTERNS = {
    IntegrationType.SOAP: {
        "patterns": [
            (r"using CoreWCF", Confidence.HIGH),
            (r"CoreWCF\.Http", Confidence.HIGH),
        ],
    },
}
