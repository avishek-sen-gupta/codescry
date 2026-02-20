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
                ),
                (r"fiber\.Ctx", Confidence.HIGH, SignalDirection.INWARD),
                (r"fiber\.New\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
