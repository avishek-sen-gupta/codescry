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
                        "Rocket library import for HTTP server framework",
                        "This code uses Rocket to interact with an HTTP server",
                    ),
                ),
                (
                    r"#\[get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rocket #[get] macro defining an inbound GET HTTP route handler",
                        "This code uses Rocket to handle inbound GET HTTP requests",
                    ),
                ),
                (
                    r"#\[post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rocket #[post] macro defining an inbound POST HTTP route handler",
                        "This code uses Rocket to handle inbound POST HTTP requests",
                    ),
                ),
                (
                    r"#\[put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rocket #[put] macro defining an inbound PUT HTTP route handler",
                        "This code uses Rocket to handle inbound PUT HTTP requests",
                    ),
                ),
                (
                    r"#\[delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rocket #[delete] macro defining an inbound DELETE HTTP route handler",
                        "This code uses Rocket to handle inbound DELETE HTTP requests",
                    ),
                ),
                (
                    r"#\[patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rocket #[patch] macro defining an inbound PATCH HTTP route handler",
                        "This code uses Rocket to handle inbound PATCH HTTP requests",
                    ),
                ),
                (
                    r"#\[route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rocket #[route] macro defining a generic inbound HTTP route handler",
                        "This code uses Rocket to handle inbound HTTP requests",
                    ),
                ),
            ],
        },
    },
)
