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
                        "TypeScript Hono import for lightweight HTTP server framework",
                        "This code uses TypeScript Hono to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"new Hono\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Hono constructor creating an HTTP application instance",
                        "This code uses TypeScript Hono to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"app\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Hono app.get() route handler accepting inbound GET requests",
                        "This code uses TypeScript Hono to handle inbound GET HTTP requests",
                    ),
                ),
                (
                    r"app\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Hono app.post() route handler accepting inbound POST requests",
                        "This code uses TypeScript Hono to handle inbound POST HTTP requests",
                    ),
                ),
            ],
        },
    },
)
