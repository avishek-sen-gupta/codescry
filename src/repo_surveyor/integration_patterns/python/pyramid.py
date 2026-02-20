"""Pyramid framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Pyramid",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from pyramid import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"from pyramid\.view import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"@view_config", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"from pyramid\.config import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
    },
)
