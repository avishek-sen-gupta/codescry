"""ServiceStack framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="ServiceStack",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"using ServiceStack",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ServiceStack namespace import enabling a ServiceStack web service",
                        "This code uses ServiceStack to expose inbound HTTP REST services",
                    ),
                ),
                (
                    r"IReturn<",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ServiceStack IReturn<T> interface marking a request DTO with its response type",
                        "This code uses ServiceStack to handle inbound HTTP service requests",
                    ),
                ),
                (
                    r"IGet\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ServiceStack IGet interface marking a request DTO for HTTP GET handling",
                        "This code uses ServiceStack to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"IPost\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ServiceStack IPost interface marking a request DTO for HTTP POST handling",
                        "This code uses ServiceStack to handle incoming HTTP POST requests",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"using ServiceStack\.OrmLite",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLite namespace import for lightweight ORM database access",
                        "This code uses ServiceStack OrmLite to query a database",
                    ),
                ),
                (
                    r"OrmLiteConnectionFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLiteConnectionFactory class creating database connections",
                        "This code uses ServiceStack OrmLite to connect to an outbound database",
                    ),
                ),
                (
                    r"IDbConnectionFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack IDbConnectionFactory interface providing database connections",
                        "This code uses ServiceStack OrmLite to connect to an outbound database",
                    ),
                ),
                (
                    r"Db\.Select<",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLite Db.Select<T> method querying rows from a database table",
                        "This code uses ServiceStack OrmLite to query an outbound database",
                    ),
                ),
                (
                    r"Db\.Insert\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLite Db.Insert method inserting a row into a database table",
                        "This code uses ServiceStack OrmLite to write to an outbound database",
                    ),
                ),
                (
                    r"Db\.Update\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLite Db.Update method updating rows in a database table",
                        "This code uses ServiceStack OrmLite to write to an outbound database",
                    ),
                ),
                (
                    r"Db\.Delete\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLite Db.Delete method deleting rows from a database table",
                        "This code uses ServiceStack OrmLite to write to an outbound database",
                    ),
                ),
                (
                    r"Db\.SingleById\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLite Db.SingleById method fetching a row by primary key",
                        "This code uses ServiceStack OrmLite to query an outbound database by ID",
                    ),
                ),
                (
                    r"Db\.LoadSelect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack OrmLite Db.LoadSelect method eagerly loading related rows from a database",
                        "This code uses ServiceStack OrmLite to query an outbound database with related data",
                    ),
                ),
                (
                    r"using ServiceStack\.Data",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ServiceStack.Data namespace import providing database connection abstractions",
                        "This code uses ServiceStack to connect to an outbound database",
                    ),
                ),
            ],
        },
    },
)
