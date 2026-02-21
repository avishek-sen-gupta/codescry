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
                        "Quarkus import for building Quarkus application components",
                        "This code uses Quarkus to interact with an HTTP endpoint",
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
                        "Quarkus Panache PanacheEntity base class for active record pattern ORM",
                        "This code uses Quarkus Panache to query a database entity",
                    ),
                ),
                (
                    r"PanacheRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Panache PanacheRepository interface for repository pattern database access",
                        "This code uses Quarkus Panache to query a database",
                    ),
                ),
                (
                    r"PanacheEntityBase",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Panache PanacheEntityBase for customized active record ORM entities",
                        "This code uses Quarkus Panache to query a database entity",
                    ),
                ),
                (
                    r"PanacheMongoEntity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Panache PanacheMongoEntity for active record MongoDB documents",
                        "This code uses Quarkus Panache to query a MongoDB database",
                    ),
                ),
                (
                    r"PanacheMongoRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Panache PanacheMongoRepository for repository pattern MongoDB access",
                        "This code uses Quarkus Panache to query a MongoDB database",
                    ),
                ),
                (
                    r"io\.quarkus\.hibernate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Hibernate ORM import for JPA-based relational database access",
                        "This code uses Quarkus Hibernate to query a relational database",
                    ),
                ),
                (
                    r"io\.quarkus\.panache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Panache import for simplified ORM-based database access",
                        "This code uses Quarkus Panache to query a database",
                    ),
                ),
                (
                    r"@NamedQuery",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus @NamedQuery annotation defining a named JPA query on an entity",
                        "This code uses Quarkus JPA to execute a named query against a database",
                    ),
                ),
                (
                    r"import io\.quarkus\.neo4j",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Neo4j import for connecting to a Neo4j graph database",
                        "This code uses Quarkus Neo4j to query a Neo4j graph database",
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
                        "Quarkus Reactive Messaging @Incoming annotation consuming messages from a channel",
                        "This code uses Quarkus Reactive Messaging to receive incoming messages from a message queue",
                    ),
                ),
                (
                    r"@Outgoing\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Quarkus Reactive Messaging @Outgoing annotation producing messages to a channel",
                        "This code uses Quarkus Reactive Messaging to send outgoing messages to a message queue",
                    ),
                ),
            ],
        },
    },
)
