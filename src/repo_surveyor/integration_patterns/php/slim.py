"""Slim framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Slim",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\$app->get\(", Confidence.HIGH),
                (r"\$app->post\(", Confidence.HIGH),
                (r"\$app->put\(", Confidence.HIGH),
                (r"\$app->delete\(", Confidence.HIGH),
                (r"\$app->patch\(", Confidence.HIGH),
                (r"\$response->withJson\(", Confidence.HIGH),
                (r"AppFactory::create\(", Confidence.HIGH),
            ],
        },
    },
)
