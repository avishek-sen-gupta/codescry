"""Warp framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Warp",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"use warp::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Warp library import for composable HTTP server framework",
                        "This code uses Warp to interact with an HTTP server",
                    ),
                ),
            ],
        },
    },
)
