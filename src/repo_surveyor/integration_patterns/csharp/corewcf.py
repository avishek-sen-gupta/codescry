"""CoreWCF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="CoreWCF",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"using CoreWCF", Confidence.HIGH),
                (r"CoreWCF\.Http", Confidence.HIGH),
            ],
        },
    },
)
