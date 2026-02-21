"""aiohttp framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="aiohttp",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from aiohttp import",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "aiohttp framework import for async HTTP client and server operations",
                        "This code uses aiohttp to interact with an HTTP endpoint",
                    ),
                ),
            ],
        },
    },
)
