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
                        "SOAP endpoint is annotated for web service",
                        "SOAP service is exposed inbound",
                    ),
                ),
                (
                    r"@PayloadRoot",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP request is mapped by payload root",
                        "SOAP requests are handled inbound",
                    ),
                ),
                (
                    r"WebServiceTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP requests are sent to remote service",
                        "SOAP service is called outbound",
                    ),
                ),
                (
                    r"org\.springframework\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP web service integration is imported",
                        "SOAP service is accessed",
                    ),
                ),
            ],
        },
    },
)
