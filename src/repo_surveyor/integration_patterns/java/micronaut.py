"""Micronaut framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Micronaut",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.micronaut",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP components are imported for server client",
                        "HTTP endpoint is accessed",
                    ),
                ),
                (
                    r"@Client\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is annotated with declarative interface",
                        "HTTP REST API is called outbound",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.micronaut\.data",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Data persistence layer is imported",
                        "Database is queried",
                    ),
                ),
                (
                    r"@MappedEntity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Entity class is mapped to database table",
                        "Database entity is defined for persistence",
                    ),
                ),
                (
                    r"@MappedProperty",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Field is mapped to database column",
                        "Field is mapped to database column",
                    ),
                ),
                (
                    r"CrudRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "CRUD repository is created for database operations",
                        "Database is accessed with CRUD queries",
                    ),
                ),
                (
                    r"PageableRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Pageable repository is created for database queries",
                        "Database is queried with pagination",
                    ),
                ),
                (
                    r"JpaRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JPA repository is created for database access",
                        "Database is queried with JPA",
                    ),
                ),
                (
                    r"io\.micronaut\.transaction",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Transaction management is imported for database control",
                        "Database queries are executed within transaction",
                    ),
                ),
                (
                    r"@R2dbcRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "R2DBC repository is annotated for reactive database",
                        "Database is queried reactively with R2DBC",
                    ),
                ),
                (
                    r"@JdbcRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JDBC repository is annotated for database access",
                        "Database is queried with JDBC",
                    ),
                ),
                (
                    r"@MongoRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB repository is annotated for database access",
                        "MongoDB database is queried",
                    ),
                ),
            ],
        },
    },
)
