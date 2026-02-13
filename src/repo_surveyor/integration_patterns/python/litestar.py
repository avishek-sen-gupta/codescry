"""Litestar framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Litestar",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from litestar import", Confidence.HIGH),
                (r"Litestar\(", Confidence.HIGH),
                (r"@get\(", Confidence.HIGH),
                (r"@post\(", Confidence.HIGH),
                (r"@put\(", Confidence.HIGH),
                (r"@delete\(", Confidence.HIGH),
            ],
        },
    },
)
