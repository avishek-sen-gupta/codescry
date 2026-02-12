"""Micronaut framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Micronaut",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.micronaut", Confidence.HIGH),
                (r"@Client\(", Confidence.HIGH),
            ],
        },
    },
)
