"""Flask framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
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
                (
                    r"@\w+\.route",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route is registered with path handler",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"from flask import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Web framework is imported for REST APIs",
                        "HTTP endpoint is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"from flask_sqlalchemy import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM extension is imported for database integration",
                        "Database is accessed via SQLAlchemy",
                    ),
                ),
                (
                    r"import flask_sqlalchemy",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM library is imported for database access",
                        "Database is accessed via SQLAlchemy",
                    ),
                ),
                (
                    r"SQLAlchemy\(app\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM instance is bound to Flask application",
                        "Database connection is opened via SQLAlchemy",
                    ),
                ),
                (
                    r"SQLAlchemy\(\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM instance is created for deferred binding",
                        "Database connection is opened via SQLAlchemy",
                    ),
                ),
                (
                    r"db\.Model",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database model is defined via ORM base class",
                        "Database model is defined for persistence",
                    ),
                ),
                (
                    r"db\.Column\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is defined for schema mapping",
                        "Database model is defined for persistence",
                    ),
                ),
                (
                    r"db\.relationship\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Model associations are defined via relationship",
                        "Database is accessed via SQLAlchemy",
                    ),
                ),
                (
                    r"db\.ForeignKey\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Foreign key is defined for database linking",
                        "Database is accessed via SQLAlchemy",
                    ),
                ),
                (
                    r"db\.session\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are handled transactionally",
                        "Database is written via SQLAlchemy",
                    ),
                ),
                (
                    r"db\.create_all\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database schema is initialized",
                        "Database is written via SQLAlchemy",
                    ),
                ),
                (
                    r"Flask-Migrate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database migrations are managed via extension",
                        "Database is accessed",
                    ),
                ),
                (
                    r"from flask_migrate import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database schema migrations are imported",
                        "Database is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"Flask-Caching",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache store is integrated via extension",
                        "Cache store is connected",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"Flask-APScheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background tasks are scheduled via extension",
                        "Scheduled task is handled by APScheduler",
                    ),
                ),
            ],
        },
    },
)
