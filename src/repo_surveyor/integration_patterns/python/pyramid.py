"""Pyramid framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Pyramid",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from pyramid import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pyramid framework import for building web applications and REST APIs",
                        "This code uses Pyramid to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"from pyramid\.view import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pyramid view module import for request handling and view registration",
                        "This code uses Pyramid to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"@view_config",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Pyramid @view_config decorator defining an inbound HTTP view endpoint",
                        "This code uses Pyramid to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"from pyramid\.config import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pyramid config module import for application configuration and routing",
                        "This code uses Pyramid to interact with an HTTP endpoint",
                    ),
                ),
            ],
        },
    },
)
