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
                (r"import io\.micronaut", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@Client\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.micronaut\.data",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"@MappedEntity", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@MappedProperty", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"CrudRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PageableRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"JpaRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"io\.micronaut\.transaction",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"@R2dbcRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@JdbcRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@MongoRepository", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
    },
)
