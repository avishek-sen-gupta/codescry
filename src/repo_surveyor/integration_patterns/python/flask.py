"""Flask framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Flask",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@\w+\.route", Confidence.HIGH),
                (r"from flask import", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Flask-Caching", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"Flask-APScheduler", Confidence.HIGH),
            ],
        },
    },
)
