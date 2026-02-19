"""Koa framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Koa",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]koa['\"]\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"new Koa\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"require\(['\"]@koa/router['\"]\)", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
