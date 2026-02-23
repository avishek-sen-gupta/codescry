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
                        "Dropwizard application is imported for REST construction",
                        "REST API is exposed inbound",
                    ),
                ),
                (
                    r"@Path\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "URI path is defined for REST resource",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"@Produces",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Response media types are specified for resource",
                        "HTTP requests are handled returning media types",
                    ),
                ),
                (
                    r"@Consumes",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Request media types are specified by annotation",
                        "HTTP requests are accepted with media types",
                    ),
                ),
            ],
        },
    },
)
