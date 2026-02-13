"""Koa framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Koa",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]koa['\"]\)", Confidence.HIGH),
                (r"new Koa\(", Confidence.HIGH),
                (r"require\(['\"]@koa/router['\"]\)", Confidence.HIGH),
            ],
        },
    },
)
