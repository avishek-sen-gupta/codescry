"""Crow framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Crow",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\bCROW_ROUTE\b", Confidence.HIGH),
                (r"crow::SimpleApp\b", Confidence.HIGH),
                (r"crow::json::wvalue\b", Confidence.HIGH),
                (r"crow::response\b", Confidence.HIGH),
            ],
        },
    },
)
