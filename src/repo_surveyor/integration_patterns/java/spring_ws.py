"""Spring WS framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Spring WS",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"@Endpoint",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring WS @Endpoint annotation defining a SOAP web service endpoint class",
                        "This code uses Spring WS to expose an inbound SOAP web service",
                    ),
                ),
                (
                    r"@PayloadRoot",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring WS @PayloadRoot annotation mapping a SOAP request by its root element",
                        "This code uses Spring WS to handle inbound SOAP web service requests",
                    ),
                ),
                (
                    r"WebServiceTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring WS WebServiceTemplate for sending SOAP requests to a remote web service",
                        "This code uses Spring WS to call an outbound SOAP web service",
                    ),
                ),
                (
                    r"org\.springframework\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Spring WS org.springframework.ws import for SOAP web service integration",
                        "This code uses Spring WS to interact with a SOAP web service",
                    ),
                ),
            ],
        },
    },
)
