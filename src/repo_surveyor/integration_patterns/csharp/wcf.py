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
                        "WCF System.ServiceModel namespace providing core WCF service model types",
                        "This code uses WCF to interact with a SOAP service model",
                    ),
                ),
                (
                    r"ChannelFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "WCF ChannelFactory<T> class creating a proxy channel to a remote SOAP service",
                        "This code uses WCF to call an outbound SOAP service via a channel factory",
                    ),
                ),
                (
                    r"BasicHttpBinding",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WCF BasicHttpBinding class configuring a basic HTTP SOAP binding",
                        "This code uses WCF to interact with a SOAP service over HTTP",
                    ),
                ),
                (
                    r"ServiceHost",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WCF ServiceHost class hosting and exposing a WCF service endpoint",
                        "This code uses WCF to expose an inbound SOAP service host",
                    ),
                ),
            ],
        },
    },
)
