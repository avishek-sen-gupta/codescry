"""Slim framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Slim",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\$app->get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\$app->post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\$app->put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\$app->delete\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\$app->patch\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\$response->withJson\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"AppFactory::create\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
