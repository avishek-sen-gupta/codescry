"""Carter framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Carter",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"CarterModule", Confidence.HIGH, SignalDirection.INWARD),
                (r"ICarterModule", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
