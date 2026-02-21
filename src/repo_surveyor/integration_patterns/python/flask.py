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
                        "Flask @app.route decorator defining an inbound HTTP REST endpoint",
                        "This code uses Flask to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"from flask import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Flask framework import for building web applications and REST APIs",
                        "This code uses Flask to interact with an HTTP endpoint",
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
                        "Flask-SQLAlchemy extension import for ORM database integration",
                        "This code uses Flask SQLAlchemy to interact with a relational database",
                    ),
                ),
                (
                    r"import flask_sqlalchemy",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy library import for database ORM access",
                        "This code uses Flask SQLAlchemy to interact with a relational database",
                    ),
                ),
                (
                    r"SQLAlchemy\(app\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy instantiation binding ORM to Flask application",
                        "This code uses Flask SQLAlchemy to connect to a relational database",
                    ),
                ),
                (
                    r"SQLAlchemy\(\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy instantiation for deferred application binding",
                        "This code uses Flask SQLAlchemy to connect to a relational database",
                    ),
                ),
                (
                    r"db\.Model",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy db.Model base class for ORM database model definition",
                        "This code uses Flask SQLAlchemy to define a database model for persistence",
                    ),
                ),
                (
                    r"db\.Column\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy db.Column field definition for database schema mapping",
                        "This code uses Flask SQLAlchemy to define a database model for persistence",
                    ),
                ),
                (
                    r"db\.relationship\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy db.relationship definition for ORM model associations",
                        "This code uses Flask SQLAlchemy to interact with a relational database",
                    ),
                ),
                (
                    r"db\.ForeignKey\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy db.ForeignKey definition for relational database linking",
                        "This code uses Flask SQLAlchemy to interact with a relational database",
                    ),
                ),
                (
                    r"db\.session\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy db.session for transactional database operations",
                        "This code uses Flask SQLAlchemy to write to a database",
                    ),
                ),
                (
                    r"db\.create_all\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-SQLAlchemy db.create_all for database schema initialisation",
                        "This code uses Flask SQLAlchemy to write to a database",
                    ),
                ),
                (
                    r"Flask-Migrate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-Migrate extension for database schema migration management",
                        "This code uses Flask to interact with a relational database",
                    ),
                ),
                (
                    r"from flask_migrate import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Flask-Migrate import for applying database schema migrations",
                        "This code uses Flask to interact with a relational database",
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
                        "Flask-Caching extension for response and data cache store integration",
                        "This code uses Flask to connect to a cache store",
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
                        "Flask-APScheduler extension for background task scheduling",
                        "This code uses Flask APScheduler to interact with a scheduled task",
                    ),
                ),
            ],
        },
    },
)
