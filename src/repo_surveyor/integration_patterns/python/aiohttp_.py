"""aiohttp framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="aiohttp",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"from aiohttp import", Confidence.MEDIUM),
            ],
        },
    },
)
