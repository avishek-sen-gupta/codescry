"""Hono framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Hono",
    import_patterns=(r"from ['\"]hono['\"]",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]hono['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Hono",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"new Hono\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP application is created via constructor",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"app\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET requests are handled by route",
                        "GET request is handled by HTTP route",
                    ),
                ),
                (
                    r"app\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST requests are handled by route",
                        "POST request is handled by HTTP route",
                    ),
                ),
            ],
        },
    },
)
