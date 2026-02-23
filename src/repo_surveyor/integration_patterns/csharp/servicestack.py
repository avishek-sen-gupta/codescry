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
                        "Web service is enabled via ServiceStack namespace",
                        "HTTP REST services are exposed with ServiceStack",
                    ),
                ),
                (
                    r"IReturn<",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Request DTO is marked with response type",
                        "HTTP service requests are handled with ServiceStack",
                    ),
                ),
                (
                    r"IGet\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP GET request is marked by IGet interface",
                        "HTTP GET requests are handled with ServiceStack",
                    ),
                ),
                (
                    r"IPost\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP POST request is marked by IPost interface",
                        "HTTP POST requests are handled with ServiceStack",
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
                        "Lightweight ORM is imported via OrmLite namespace",
                        "Database is queried with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"OrmLiteConnectionFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connections are created by OrmLiteConnectionFactory",
                        "Database is connected with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"IDbConnectionFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connections are provided by IDbConnectionFactory interface",
                        "Database is connected with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"Db\.Select<",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database rows are queried by Db.Select method",
                        "Database is queried with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"Db\.Insert\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database row is inserted by Db.Insert method",
                        "Database is written with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"Db\.Update\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database rows are updated by Db.Update method",
                        "Database is written with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"Db\.Delete\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database rows are deleted by Db.Delete method",
                        "Database is written with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"Db\.SingleById\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database row is fetched by Db.SingleById method",
                        "Database is queried by ID with ServiceStack OrmLite",
                    ),
                ),
                (
                    r"Db\.LoadSelect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Related database rows are loaded by Db.LoadSelect method",
                        "Database is queried with related data",
                    ),
                ),
                (
                    r"using ServiceStack\.Data",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connections are abstracted via ServiceStack.Data namespace",
                        "Database is connected with ServiceStack",
                    ),
                ),
            ],
        },
    },
)
