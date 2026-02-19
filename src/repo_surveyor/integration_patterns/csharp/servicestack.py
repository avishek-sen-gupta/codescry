"""ServiceStack framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="ServiceStack",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"using ServiceStack", Confidence.HIGH, SignalDirection.INWARD),
                (r"IReturn<", Confidence.HIGH, SignalDirection.INWARD),
                (r"IGet\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"IPost\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"using ServiceStack\.OrmLite", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"OrmLiteConnectionFactory", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"IDbConnectionFactory", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Db\.Select<", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Db\.Insert\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Db\.Update\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Db\.Delete\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Db\.SingleById\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Db\.LoadSelect\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"using ServiceStack\.Data", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
    },
)
