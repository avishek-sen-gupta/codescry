"""Rocket framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Rocket",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"use rocket::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP server framework is imported",
                        "HTTP server is configured",
                    ),
                ),
                (
                    r"#\[get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET HTTP route is defined by macro",
                        "GET route is registered for inbound HTTP requests",
                    ),
                ),
                (
                    r"#\[post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST HTTP route is defined by macro",
                        "POST route is registered for inbound HTTP requests",
                    ),
                ),
                (
                    r"#\[put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT HTTP route is defined by macro",
                        "PUT route is registered for inbound HTTP requests",
                    ),
                ),
                (
                    r"#\[delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE HTTP route is defined by macro",
                        "DELETE route is registered for inbound HTTP requests",
                    ),
                ),
                (
                    r"#\[patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PATCH HTTP route is defined by macro",
                        "PATCH route is registered for inbound HTTP requests",
                    ),
                ),
                (
                    r"#\[route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route is defined by generic macro",
                        "HTTP request is handled by route",
                    ),
                ),
            ],
        },
    },
)
