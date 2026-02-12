"""Warp framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Warp",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"use warp::", Confidence.HIGH),
            ],
        },
    },
)
