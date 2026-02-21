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
                        "Fiber gofiber/fiber package import for the Fiber HTTP framework",
                        "This code uses Fiber to interact with an HTTP web framework",
                    ),
                ),
                (
                    r"fiber\.Ctx",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Fiber fiber.Ctx struct representing an inbound HTTP request context",
                        "This code uses Fiber to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"fiber\.New\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Fiber fiber.New function creating a new Fiber HTTP application instance",
                        "This code uses Fiber to expose an inbound HTTP server",
                    ),
                ),
            ],
        },
    },
)
