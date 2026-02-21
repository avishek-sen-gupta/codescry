"""Echo framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Echo",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/labstack/echo"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Echo labstack/echo package import for the Echo HTTP framework",
                        "This code uses Echo to interact with an HTTP web framework",
                    ),
                ),
                (
                    r"echo\.Context",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Echo echo.Context interface representing an inbound HTTP request context",
                        "This code uses Echo to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"echo\.New\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Echo echo.New function creating a new Echo HTTP server instance",
                        "This code uses Echo to expose an inbound HTTP server",
                    ),
                ),
            ],
        },
    },
)
