"""Flask framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Flask",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@\w+\.route", Confidence.HIGH),
                (r"from flask import", Confidence.HIGH),
            ],
        },
    },
)
