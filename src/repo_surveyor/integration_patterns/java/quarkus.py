"""Quarkus framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Quarkus",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.quarkus", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"PanacheEntity", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PanacheRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PanacheEntityBase", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PanacheMongoEntity", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PanacheMongoRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"io\.quarkus\.hibernate", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"io\.quarkus\.panache", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@NamedQuery", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import io\.quarkus\.neo4j", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@Incoming\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Outgoing\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
    },
)
