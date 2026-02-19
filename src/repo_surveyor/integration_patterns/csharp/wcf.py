"""WCF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="WCF",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"System\.ServiceModel", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"ChannelFactory", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"BasicHttpBinding", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"ServiceHost", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
