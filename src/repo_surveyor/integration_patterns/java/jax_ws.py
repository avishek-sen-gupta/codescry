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
                        "JAX-WS javax.xml.ws import for building SOAP web service clients or endpoints",
                        "This code uses JAX-WS to interact with a SOAP web service",
                    ),
                ),
                (
                    r"jakarta\.xml\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "JAX-WS jakarta.xml.ws import for building SOAP web service clients or endpoints",
                        "This code uses JAX-WS to interact with a SOAP web service",
                    ),
                ),
                (
                    r"@WebServiceClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JAX-WS @WebServiceClient annotation generated for a SOAP service client stub",
                        "This code uses JAX-WS to call an outbound SOAP web service",
                    ),
                ),
                (
                    r"Service\.create",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JAX-WS Service.create call for instantiating a SOAP service client",
                        "This code uses JAX-WS to connect to an outbound SOAP web service",
                    ),
                ),
            ],
        },
    },
)
