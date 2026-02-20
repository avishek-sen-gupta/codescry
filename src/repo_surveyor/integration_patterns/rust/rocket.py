"""Rocket framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Rocket",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"use rocket::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"#\[get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[delete\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[patch\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[route\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
