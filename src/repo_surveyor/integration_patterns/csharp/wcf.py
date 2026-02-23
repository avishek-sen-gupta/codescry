"""WCF framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="WCF",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"System\.ServiceModel",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WCF service model is imported",
                        "SOAP service is accessed via WCF model",
                    ),
                ),
                (
                    r"ChannelFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP proxy is created via ChannelFactory",
                        "SOAP service is called via WCF channel factory",
                    ),
                ),
                (
                    r"BasicHttpBinding",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP binding is configured for SOAP",
                        "SOAP service is accessed over HTTP",
                    ),
                ),
                (
                    r"ServiceHost",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WCF service is hosted by ServiceHost",
                        "SOAP service is hosted for inbound requests",
                    ),
                ),
            ],
        },
    },
)
