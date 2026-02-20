"""Drogon framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Drogon",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\bHttpController\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bHttpSimpleController\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bHttpApiController\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bHttpResponse\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\bWebSocketController\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
