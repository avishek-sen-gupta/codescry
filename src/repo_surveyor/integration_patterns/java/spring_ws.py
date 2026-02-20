"""Spring WS framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Spring WS",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"@Endpoint", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PayloadRoot", Confidence.HIGH, SignalDirection.INWARD),
                (r"WebServiceTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"org\.springframework\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
    },
)
