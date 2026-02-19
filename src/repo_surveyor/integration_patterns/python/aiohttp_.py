"""aiohttp framework integration patterns."""

from ..types import (
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
                (r"from aiohttp import", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
