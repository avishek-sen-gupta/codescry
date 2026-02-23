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
                        "Echo framework is imported for HTTP handling",
                        "HTTP framework is accessed",
                    ),
                ),
                (
                    r"echo\.Context",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request context is represented by Context",
                        "HTTP requests are handled incoming",
                    ),
                ),
                (
                    r"echo\.New\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Echo server instance is created",
                        "HTTP server is exposed inbound",
                    ),
                ),
            ],
        },
    },
)
