"""Micronaut framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Micronaut",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"import io\.micronaut", Confidence.HIGH),
                (r"@Client\(", Confidence.HIGH),
            ],
        },
    },
)
