"""Fiber framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Fiber",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/gofiber/fiber"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Fiber framework is imported for HTTP handling",
                        "HTTP framework is accessed",
                    ),
                ),
                (
                    r"fiber\.Ctx",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request context is represented by Ctx",
                        "HTTP requests are handled incoming",
                    ),
                ),
                (
                    r"fiber\.New\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Fiber application instance is created",
                        "HTTP server is exposed inbound",
                    ),
                ),
            ],
        },
    },
)
