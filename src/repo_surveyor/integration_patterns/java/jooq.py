"""jOOQ framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="jOOQ",
    import_patterns=(r"import org\.jooq",),
    patterns={
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.jooq",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ import for building type-safe SQL queries",
                        "This code uses jOOQ to query a relational database",
                    ),
                ),
                (
                    r"DSLContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ DSLContext for constructing and executing SQL queries",
                        "This code uses jOOQ DSLContext to execute SQL queries against a database",
                    ),
                ),
                (
                    r"\.insertInto\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .insertInto() for constructing a SQL INSERT statement",
                        "This code uses jOOQ to write rows to a database table",
                    ),
                ),
                (
                    r"\.select\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .select() for constructing a SQL SELECT query",
                        "This code uses jOOQ to query rows from a database table",
                    ),
                ),
                (
                    r"\.selectFrom\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .selectFrom() for constructing a SQL SELECT FROM query on a table",
                        "This code uses jOOQ to query rows from a database table",
                    ),
                ),
                (
                    r"\.update\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .update() for constructing a SQL UPDATE statement",
                        "This code uses jOOQ to update rows in a database table",
                    ),
                ),
                (
                    r"\.deleteFrom\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .deleteFrom() for constructing a SQL DELETE statement",
                        "This code uses jOOQ to delete rows from a database table",
                    ),
                ),
                (
                    r"\.fetch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .fetch() for executing a SQL query and retrieving all results",
                        "This code uses jOOQ to query and fetch results from a database",
                    ),
                ),
                (
                    r"\.fetchOne\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .fetchOne() for executing a SQL query and retrieving a single result",
                        "This code uses jOOQ to query a single row from a database",
                    ),
                ),
                (
                    r"\.execute\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "jOOQ .execute() for executing a SQL DML statement",
                        "This code uses jOOQ to execute a SQL statement against a database",
                    ),
                ),
            ],
        },
    },
)
