"""jOOQ framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="jOOQ",
    import_patterns=(r"import org\.jooq",),
    patterns={
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import org\.jooq", Confidence.HIGH),
                (r"DSLContext", Confidence.HIGH),
                (r"\.insertInto\(", Confidence.HIGH),
                (r"\.select\(", Confidence.HIGH),
                (r"\.selectFrom\(", Confidence.HIGH),
                (r"\.update\(", Confidence.HIGH),
                (r"\.deleteFrom\(", Confidence.HIGH),
                (r"\.fetch\(", Confidence.HIGH),
                (r"\.fetchOne\(", Confidence.HIGH),
                (r"\.execute\(", Confidence.MEDIUM),
            ],
        },
    },
)
