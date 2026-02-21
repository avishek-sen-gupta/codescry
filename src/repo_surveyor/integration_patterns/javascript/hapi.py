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
                        "Node.js Hapi framework require for HTTP server",
                        "This code uses Node.js Hapi to handle inbound HTTP server requests",
                    ),
                ),
                (
                    r"Hapi\.server\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Hapi Hapi.server call for HTTP server instantiation",
                        "This code uses Node.js Hapi to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"server\.route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Hapi server.route call for HTTP route registration",
                        "This code uses Node.js Hapi to handle inbound HTTP routes",
                    ),
                ),
            ],
        },
    },
)
