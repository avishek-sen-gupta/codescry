"""JAX-WS framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="JAX-WS",
    patterns={
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"javax\.xml\.ws", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"jakarta\.xml\.ws", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@WebServiceClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Service\.create", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
    },
)
