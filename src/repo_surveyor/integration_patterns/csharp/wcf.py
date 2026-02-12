"""WCF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="WCF",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"System\.ServiceModel", Confidence.HIGH),
                (r"ChannelFactory", Confidence.HIGH),
                (r"BasicHttpBinding", Confidence.HIGH),
                (r"ServiceHost", Confidence.HIGH),
            ],
        },
    },
)
