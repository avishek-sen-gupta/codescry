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
                        "CXF framework is imported for web services",
                        "HTTP endpoint is accessed",
                    ),
                ),
                (
                    r"JaxWsServerFactoryBean",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-WS service is published with server factory",
                        "REST API is exposed for inbound requests",
                    ),
                ),
                (
                    r"JaxRsServerFactoryBean",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-RS service is published with server factory",
                        "REST API is exposed for inbound requests",
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
                        "JAX-WS integration is imported for SOAP services",
                        "SOAP web service is accessed",
                    ),
                ),
                (
                    r"CxfEndpoint",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "CXF endpoint is configured for web service",
                        "SOAP endpoint is accessed",
                    ),
                ),
            ],
        },
    },
)
