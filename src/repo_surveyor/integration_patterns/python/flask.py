"""Flask framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Flask",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@\w+\.route", Confidence.HIGH, SignalDirection.INWARD),
                (r"from flask import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"from flask_sqlalchemy import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"import flask_sqlalchemy", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SQLAlchemy\(app\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SQLAlchemy\(\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.Model", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.Column\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.relationship\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.ForeignKey\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.session\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.create_all\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Flask-Migrate", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"from flask_migrate import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Flask-Caching", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"Flask-APScheduler", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
