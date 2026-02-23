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
                        "HTTP framework is imported",
                        "HTTP framework is accessed",
                    ),
                ),
                (
                    r"gin\.Context",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request context is represented",
                        "HTTP requests are handled incoming",
                    ),
                ),
                (
                    r"gin\.Default\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP engine is created with middleware",
                        "HTTP server is exposed with middleware",
                    ),
                ),
                (
                    r"gin\.New\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP engine is created",
                        "HTTP server is exposed inbound",
                    ),
                ),
            ],
        },
    },
)
