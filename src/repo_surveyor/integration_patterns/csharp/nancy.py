"""Nancy framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Nancy",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"NancyModule",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Route handlers are defined with NancyModule",
                        "HTTP REST route handlers are exposed inbound",
                    ),
                ),
                (
                    r"Get\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is registered for HTTP requests",
                        "HTTP GET request is handled",
                    ),
                ),
                (
                    r"Post\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is registered for HTTP requests",
                        "HTTP POST request is handled",
                    ),
                ),
            ],
        },
    },
)
