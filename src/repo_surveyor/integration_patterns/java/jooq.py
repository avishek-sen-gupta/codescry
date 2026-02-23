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
                        "SQL query library is imported for type-safe operations",
                        "Database is queried via jOOQ",
                    ),
                ),
                (
                    r"DSLContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL queries are constructed with type-safe DSL",
                        "SQL queries are executed against database",
                    ),
                ),
                (
                    r"\.insertInto\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL INSERT statement is constructed with table target",
                        "Database rows are written to table",
                    ),
                ),
                (
                    r"\.select\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL SELECT query is constructed with field selection",
                        "Database rows are queried from table",
                    ),
                ),
                (
                    r"\.selectFrom\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL SELECT query is constructed from table source",
                        "Database rows are queried from table",
                    ),
                ),
                (
                    r"\.update\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL UPDATE statement is constructed with table target",
                        "Database rows are updated in table",
                    ),
                ),
                (
                    r"\.deleteFrom\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL DELETE statement is constructed with table target",
                        "Database rows are deleted from table",
                    ),
                ),
                (
                    r"\.fetch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed with all results fetched",
                        "Database results are queried and fetched",
                    ),
                ),
                (
                    r"\.fetchOne\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed with single result fetched",
                        "Database row is queried and fetched",
                    ),
                ),
                (
                    r"\.execute\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "SQL statement is executed against database",
                        "SQL statement is executed against database",
                    ),
                ),
            ],
        },
    },
)
