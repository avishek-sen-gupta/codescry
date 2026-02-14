"""Micronaut framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Micronaut",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.micronaut", Confidence.HIGH),
                (r"@Client\(", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import io\.micronaut\.data", Confidence.HIGH),
                (r"@MappedEntity", Confidence.HIGH),
                (r"@MappedProperty", Confidence.HIGH),
                (r"CrudRepository", Confidence.HIGH),
                (r"PageableRepository", Confidence.HIGH),
                (r"JpaRepository", Confidence.HIGH),
                (r"io\.micronaut\.transaction", Confidence.HIGH),
                (r"@R2dbcRepository", Confidence.HIGH),
                (r"@JdbcRepository", Confidence.HIGH),
                (r"@MongoRepository", Confidence.HIGH),
            ],
        },
    },
)
