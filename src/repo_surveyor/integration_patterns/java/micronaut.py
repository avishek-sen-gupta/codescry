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
                        "Micronaut import for building Micronaut HTTP server or client components",
                        "This code uses Micronaut to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"@Client\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut @Client annotation defining a declarative HTTP client",
                        "This code uses Micronaut to call an outbound HTTP REST API",
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
                        "Micronaut Data import for accessing Micronaut's data persistence layer",
                        "This code uses Micronaut Data to query a database",
                    ),
                ),
                (
                    r"@MappedEntity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data @MappedEntity annotation mapping a class to a database table",
                        "This code uses Micronaut Data to define a database entity for persistence",
                    ),
                ),
                (
                    r"@MappedProperty",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data @MappedProperty annotation mapping a field to a database column",
                        "This code uses Micronaut Data to map a field to a database column",
                    ),
                ),
                (
                    r"CrudRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data CrudRepository interface for basic database CRUD operations",
                        "This code uses Micronaut Data to execute CRUD queries against a database",
                    ),
                ),
                (
                    r"PageableRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data PageableRepository interface for paginated database queries",
                        "This code uses Micronaut Data to execute paginated queries against a database",
                    ),
                ),
                (
                    r"JpaRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data JpaRepository interface for JPA-based database access",
                        "This code uses Micronaut Data JPA to query a relational database",
                    ),
                ),
                (
                    r"io\.micronaut\.transaction",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut transaction management import for controlling database transactions",
                        "This code uses Micronaut to execute database queries within a transaction",
                    ),
                ),
                (
                    r"@R2dbcRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data @R2dbcRepository annotation for reactive R2DBC database access",
                        "This code uses Micronaut Data R2DBC to reactively query a relational database",
                    ),
                ),
                (
                    r"@JdbcRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data @JdbcRepository annotation for JDBC-based database access",
                        "This code uses Micronaut Data JDBC to query a relational database",
                    ),
                ),
                (
                    r"@MongoRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Micronaut Data @MongoRepository annotation for MongoDB database access",
                        "This code uses Micronaut Data to query a MongoDB database",
                    ),
                ),
            ],
        },
    },
)
