"""Apache CXF framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Apache CXF",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.apache\.cxf",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Apache CXF import for building CXF-based web service clients or servers",
                        "This code uses Apache CXF to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"JaxWsServerFactoryBean",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Apache CXF JaxWsServerFactoryBean for publishing a JAX-WS SOAP service endpoint",
                        "This code uses Apache CXF to expose an inbound REST API",
                    ),
                ),
                (
                    r"JaxRsServerFactoryBean",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Apache CXF JaxRsServerFactoryBean for publishing a JAX-RS REST service endpoint",
                        "This code uses Apache CXF to expose an inbound REST API",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"org\.apache\.cxf\.jaxws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Apache CXF org.apache.cxf.jaxws import for JAX-WS SOAP service integration",
                        "This code uses Apache CXF to interact with a SOAP web service",
                    ),
                ),
                (
                    r"CxfEndpoint",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Apache CXF CxfEndpoint for configuring a CXF web service endpoint",
                        "This code uses Apache CXF to interact with a SOAP endpoint",
                    ),
                ),
            ],
        },
    },
)
