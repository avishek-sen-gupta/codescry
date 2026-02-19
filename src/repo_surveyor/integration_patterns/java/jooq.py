"""jOOQ framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="jOOQ",
    import_patterns=(r"import org\.jooq",),
    patterns={
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import org\.jooq", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"DSLContext", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.insertInto\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.select\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.selectFrom\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.update\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.deleteFrom\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.fetch\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.fetchOne\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\.execute\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
    },
)
