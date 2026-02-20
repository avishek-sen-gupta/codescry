"""Play framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Play",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import play\.api\.mvc", Confidence.HIGH, SignalDirection.INWARD),
                (r"Action\s*\{", Confidence.HIGH, SignalDirection.INWARD),
                (r"Action\.async\s*\{", Confidence.HIGH, SignalDirection.INWARD),
                (r"Ok\(", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"Json\.toJson\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"def\s+\w+.*=\s*Action", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import play\.api\.db", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import anorm\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SQL\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"import play\.api\.libs\.streams",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"WebSocket\.accept", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"import play\.api\.cache",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"@Cached", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
