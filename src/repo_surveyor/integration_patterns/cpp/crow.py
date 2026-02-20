"""Crow framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Crow",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\bCROW_ROUTE\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"crow::SimpleApp\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"crow::json::wvalue\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"crow::response\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
