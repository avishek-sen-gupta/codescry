"""Gin framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Gin",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/gin-gonic/gin"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Gin gin-gonic/gin package import for the Gin HTTP framework",
                        "This code uses Gin to interact with an HTTP web framework",
                    ),
                ),
                (
                    r"gin\.Context",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Gin gin.Context struct representing an inbound HTTP request context",
                        "This code uses Gin to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"gin\.Default\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Gin gin.Default function creating a Gin engine with default middleware",
                        "This code uses Gin to expose an inbound HTTP server with default middleware",
                    ),
                ),
                (
                    r"gin\.New\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Gin gin.New function creating a bare Gin HTTP engine instance",
                        "This code uses Gin to expose an inbound HTTP server",
                    ),
                ),
            ],
        },
    },
)
