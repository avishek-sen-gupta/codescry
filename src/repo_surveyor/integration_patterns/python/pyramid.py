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
                        "Web application is imported for REST API building",
                        "HTTP endpoint is accessed for communication",
                    ),
                ),
                (
                    r"from pyramid\.view import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Request handling is imported for view registration",
                        "HTTP endpoint is accessed for communication",
                    ),
                ),
                (
                    r"@view_config",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP view is registered with route configuration",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"from pyramid\.config import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Application configuration is imported for routing setup",
                        "HTTP endpoint is accessed for communication",
                    ),
                ),
            ],
        },
    },
)
