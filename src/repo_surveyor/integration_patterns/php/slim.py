"""Slim framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
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
                (
                    r"\$app->get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP GET route is defined for inbound requests",
                        "HTTP GET requests are handled with Slim",
                    ),
                ),
                (
                    r"\$app->post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP POST route is defined for inbound requests",
                        "HTTP POST requests are handled with Slim",
                    ),
                ),
                (
                    r"\$app->put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP PUT route is defined for inbound requests",
                        "HTTP PUT requests are handled with Slim",
                    ),
                ),
                (
                    r"\$app->delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP DELETE route is defined for inbound requests",
                        "HTTP DELETE requests are handled with Slim",
                    ),
                ),
                (
                    r"\$app->patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP PATCH route is defined for inbound requests",
                        "HTTP PATCH requests are handled with Slim",
                    ),
                ),
                (
                    r"\$response->withJson\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JSON response is sent to inbound request",
                        "HTTP endpoints are exposed returning JSON responses",
                    ),
                ),
                (
                    r"AppFactory::create\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Slim application is bootstrapped for inbound requests",
                        "HTTP application is exposed with Slim AppFactory",
                    ),
                ),
            ],
        },
    },
)
