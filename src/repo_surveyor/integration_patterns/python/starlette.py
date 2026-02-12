"""Starlette framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Starlette",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"from starlette", Confidence.MEDIUM),
            ],
        },
    },
)
