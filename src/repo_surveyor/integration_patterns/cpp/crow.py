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
                        "HTTP route handler is defined by CROW_ROUTE macro",
                        "HTTP requests are handled for inbound traffic",
                    ),
                ),
                (
                    r"crow::SimpleApp\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server application is created by SimpleApp class",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"crow::json::wvalue\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JSON HTTP response is built by json::wvalue class",
                        "HTTP requests are handled with JSON responses",
                    ),
                ),
                (
                    r"crow::response\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is constructed by response class",
                        "HTTP requests are handled for inbound traffic",
                    ),
                ),
            ],
        },
    },
)
