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
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"from flask_sqlalchemy import", Confidence.HIGH),
                (r"import flask_sqlalchemy", Confidence.HIGH),
                (r"SQLAlchemy\(app\)", Confidence.HIGH),
                (r"SQLAlchemy\(\)", Confidence.HIGH),
                (r"db\.Model", Confidence.HIGH),
                (r"db\.Column\(", Confidence.HIGH),
                (r"db\.relationship\(", Confidence.HIGH),
                (r"db\.ForeignKey\(", Confidence.HIGH),
                (r"db\.session\.", Confidence.HIGH),
                (r"db\.create_all\(", Confidence.HIGH),
                (r"Flask-Migrate", Confidence.HIGH),
                (r"from flask_migrate import", Confidence.HIGH),
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
