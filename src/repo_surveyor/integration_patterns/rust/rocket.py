"""Rocket framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Rocket",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"use rocket::", Confidence.HIGH),
                (r"#\[get\(", Confidence.HIGH),
                (r"#\[post\(", Confidence.HIGH),
                (r"#\[put\(", Confidence.HIGH),
                (r"#\[delete\(", Confidence.HIGH),
                (r"#\[patch\(", Confidence.HIGH),
                (r"#\[route\(", Confidence.HIGH),
            ],
        },
    },
)
