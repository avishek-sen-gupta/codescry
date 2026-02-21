"""Dropwizard framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Dropwizard",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.dropwizard",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Dropwizard import for building a Dropwizard REST application",
                        "This code uses Dropwizard to expose an inbound REST API",
                    ),
                ),
                (
                    r"@Path\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Dropwizard @Path annotation defining the URI path for a REST resource",
                        "This code uses Dropwizard to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@Produces",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Dropwizard @Produces annotation specifying the response media types for a resource",
                        "This code uses Dropwizard to handle inbound HTTP requests returning specific media types",
                    ),
                ),
                (
                    r"@Consumes",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Dropwizard @Consumes annotation specifying the accepted request media types",
                        "This code uses Dropwizard to accept inbound HTTP requests with specific media types",
                    ),
                ),
            ],
        },
    },
)
