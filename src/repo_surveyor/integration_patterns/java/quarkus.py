"""Quarkus framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Quarkus",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.quarkus", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"PanacheEntity", Confidence.HIGH),
                (r"PanacheRepository", Confidence.HIGH),
                (r"PanacheEntityBase", Confidence.HIGH),
                (r"PanacheMongoEntity", Confidence.HIGH),
                (r"PanacheMongoRepository", Confidence.HIGH),
                (r"io\.quarkus\.hibernate", Confidence.HIGH),
                (r"io\.quarkus\.panache", Confidence.HIGH),
                (r"@NamedQuery", Confidence.HIGH),
                (r"import io\.quarkus\.neo4j", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@Incoming\(", Confidence.HIGH),
                (r"@Outgoing\(", Confidence.HIGH),
            ],
        },
    },
)
