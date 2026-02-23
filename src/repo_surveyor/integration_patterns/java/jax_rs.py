"""JAX-RS framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="JAX-RS",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"@GET\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET resource method is annotated for HTTP",
                        "HTTP GET request is handled inbound",
                    ),
                ),
                (
                    r"@POST\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST resource method is annotated for HTTP",
                        "HTTP POST request is handled inbound",
                    ),
                ),
                (
                    r"@PUT\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT resource method is annotated for HTTP",
                        "HTTP PUT request is handled inbound",
                    ),
                ),
                (
                    r"@DELETE\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE resource method is annotated for HTTP",
                        "HTTP DELETE request is handled inbound",
                    ),
                ),
                (
                    r"@Path\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "URI path is defined for REST resource",
                        "REST API endpoint is exposed inbound",
                    ),
                ),
                (
                    r"@Produces",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Media types are specified for resource production",
                        "HTTP response is returned with media types",
                    ),
                ),
                (
                    r"@Consumes",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Media types are specified for resource consumption",
                        "HTTP request is accepted with media types",
                    ),
                ),
            ],
        },
    },
)
