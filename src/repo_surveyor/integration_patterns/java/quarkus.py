"""Quarkus framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Quarkus",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.quarkus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Quarkus components are imported for application building",
                        "HTTP endpoint is accessed via Quarkus",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"PanacheEntity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Active record ORM is enabled via base entity",
                        "Database entity is queried via Quarkus Panache",
                    ),
                ),
                (
                    r"PanacheRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database repository is created with interface pattern",
                        "Database is queried via Quarkus Panache",
                    ),
                ),
                (
                    r"PanacheEntityBase",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Active record ORM is customized via base entity",
                        "Database entity is queried via Quarkus Panache",
                    ),
                ),
                (
                    r"PanacheMongoEntity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB document is modeled with active record",
                        "MongoDB database is queried via Quarkus Panache",
                    ),
                ),
                (
                    r"PanacheMongoRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB repository is created for data access",
                        "MongoDB database is queried via Quarkus Panache",
                    ),
                ),
                (
                    r"io\.quarkus\.hibernate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Hibernate ORM is imported for database access",
                        "Database is queried via Quarkus Hibernate",
                    ),
                ),
                (
                    r"io\.quarkus\.panache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM database access is imported via Panache",
                        "Database is queried via Quarkus Panache",
                    ),
                ),
                (
                    r"@NamedQuery",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Named JPA query is annotated on entity",
                        "Named query is executed against database via Quarkus JPA",
                    ),
                ),
                (
                    r"import io\.quarkus\.neo4j",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j database is imported for graph access",
                        "Neo4j database is queried via Quarkus",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"@Incoming\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Message is consumed from channel via annotation",
                        "Messages are received from queue via Quarkus Reactive Messaging",
                    ),
                ),
                (
                    r"@Outgoing\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Message is produced to channel via annotation",
                        "Messages are sent to queue via Quarkus Reactive Messaging",
                    ),
                ),
            ],
        },
    },
)
