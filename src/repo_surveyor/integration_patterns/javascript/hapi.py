"""Hapi framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Hapi",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]@hapi/hapi['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Hapi",
                        "HTTP server is handled for inbound requests",
                    ),
                ),
                (
                    r"Hapi\.server\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is instantiated for request handling",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"server\.route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route is registered for request handling",
                        "HTTP route is handled for inbound requests",
                    ),
                ),
            ],
        },
    },
)
