"""Helidon framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Helidon",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.helidon",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Helidon import for building a Helidon HTTP server or service",
                        "This code uses Helidon to expose an inbound HTTP endpoint",
                    ),
                ),
                (
                    r"WebServer\.builder",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Helidon WebServer.builder call for configuring and starting an HTTP server",
                        "This code uses Helidon to expose an inbound REST API",
                    ),
                ),
                (
                    r"HttpRules",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Helidon HttpRules for registering HTTP route handlers",
                        "This code uses Helidon to handle inbound HTTP requests",
                    ),
                ),
            ],
        },
    },
)
