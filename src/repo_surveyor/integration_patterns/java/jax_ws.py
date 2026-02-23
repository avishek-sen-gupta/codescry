"""JAX-WS framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="JAX-WS",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"javax\.xml\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP web services are imported for endpoint building",
                        "SOAP web service is accessed via client",
                    ),
                ),
                (
                    r"jakarta\.xml\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP web services are imported for client building",
                        "SOAP web service is accessed via client",
                    ),
                ),
                (
                    r"@WebServiceClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP client stub is annotated for generation",
                        "SOAP web service is called outbound",
                    ),
                ),
                (
                    r"Service\.create",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP service client is instantiated",
                        "SOAP web service connection is opened outbound",
                    ),
                ),
            ],
        },
    },
)
