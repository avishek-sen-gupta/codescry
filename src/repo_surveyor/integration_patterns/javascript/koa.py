"""Koa framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Koa",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]koa['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Koa",
                        "HTTP server is handled for inbound requests",
                    ),
                ),
                (
                    r"new Koa\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP application is instantiated with Koa",
                        "HTTP server is exposed for inbound application",
                    ),
                ),
                (
                    r"require\(['\"]@koa/router['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP routes are handled by Koa router",
                        "HTTP route is handled for inbound requests",
                    ),
                ),
            ],
        },
    },
)
