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
                        "JAX-RS @GET annotation defining an HTTP GET resource method",
                        "This code uses JAX-RS to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"@POST\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-RS @POST annotation defining an HTTP POST resource method",
                        "This code uses JAX-RS to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"@PUT\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-RS @PUT annotation defining an HTTP PUT resource method",
                        "This code uses JAX-RS to handle incoming HTTP PUT requests",
                    ),
                ),
                (
                    r"@DELETE\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-RS @DELETE annotation defining an HTTP DELETE resource method",
                        "This code uses JAX-RS to handle incoming HTTP DELETE requests",
                    ),
                ),
                (
                    r"@Path\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-RS @Path annotation defining the URI path for a REST resource",
                        "This code uses JAX-RS to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@Produces",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-RS @Produces annotation specifying the media types a resource method can return",
                        "This code uses JAX-RS to handle inbound HTTP requests returning specific media types",
                    ),
                ),
                (
                    r"@Consumes",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JAX-RS @Consumes annotation specifying the media types a resource method accepts",
                        "This code uses JAX-RS to accept inbound HTTP requests with specific media types",
                    ),
                ),
            ],
        },
    },
)
