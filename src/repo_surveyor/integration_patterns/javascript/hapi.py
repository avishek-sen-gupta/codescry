"""Hapi framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Hapi",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]@hapi/hapi['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"Hapi\.server\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"server\.route\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
