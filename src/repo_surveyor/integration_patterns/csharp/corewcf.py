"""CoreWCF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="CoreWCF",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"using CoreWCF", Confidence.HIGH, SignalDirection.INWARD),
                (r"CoreWCF\.Http", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
