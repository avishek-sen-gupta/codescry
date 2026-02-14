"""ServiceStack framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="ServiceStack",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"using ServiceStack", Confidence.HIGH),
                (r"IReturn<", Confidence.HIGH),
                (r"IGet\b", Confidence.HIGH),
                (r"IPost\b", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"using ServiceStack\.OrmLite", Confidence.HIGH),
                (r"OrmLiteConnectionFactory", Confidence.HIGH),
                (r"IDbConnectionFactory", Confidence.HIGH),
                (r"Db\.Select<", Confidence.HIGH),
                (r"Db\.Insert\(", Confidence.HIGH),
                (r"Db\.Update\(", Confidence.HIGH),
                (r"Db\.Delete\(", Confidence.HIGH),
                (r"Db\.SingleById\(", Confidence.HIGH),
                (r"Db\.LoadSelect\(", Confidence.HIGH),
                (r"using ServiceStack\.Data", Confidence.HIGH),
            ],
        },
    },
)
