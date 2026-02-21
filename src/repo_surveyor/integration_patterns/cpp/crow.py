"""Crow framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Crow",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"\bCROW_ROUTE\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Crow CROW_ROUTE macro defining an inbound HTTP route handler",
                        "This code uses Crow to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"crow::SimpleApp\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Crow SimpleApp class creating an HTTP server application",
                        "This code uses Crow to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"crow::json::wvalue\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Crow json::wvalue class building a JSON HTTP response",
                        "This code uses Crow to handle inbound HTTP requests with JSON responses",
                    ),
                ),
                (
                    r"crow::response\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Crow response class constructing an HTTP response",
                        "This code uses Crow to handle inbound HTTP requests",
                    ),
                ),
            ],
        },
    },
)
