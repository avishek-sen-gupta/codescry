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
                        "Node.js Koa framework require for HTTP server",
                        "This code uses Node.js Koa to handle inbound HTTP server requests",
                    ),
                ),
                (
                    r"new Koa\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Koa new Koa() instantiation for HTTP application",
                        "This code uses Node.js Koa to expose an inbound HTTP server application",
                    ),
                ),
                (
                    r"require\(['\"]@koa/router['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Node.js Koa router require for HTTP route handling",
                        "This code uses Node.js Koa to handle inbound HTTP routes",
                    ),
                ),
            ],
        },
    },
)
