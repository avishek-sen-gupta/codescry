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
                        "Slim $app->get defining an inbound HTTP GET route",
                        "This code uses Slim routing to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"\$app->post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Slim $app->post defining an inbound HTTP POST route",
                        "This code uses Slim routing to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"\$app->put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Slim $app->put defining an inbound HTTP PUT route",
                        "This code uses Slim routing to handle incoming HTTP PUT requests",
                    ),
                ),
                (
                    r"\$app->delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Slim $app->delete defining an inbound HTTP DELETE route",
                        "This code uses Slim routing to handle incoming HTTP DELETE requests",
                    ),
                ),
                (
                    r"\$app->patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Slim $app->patch defining an inbound HTTP PATCH route",
                        "This code uses Slim routing to handle incoming HTTP PATCH requests",
                    ),
                ),
                (
                    r"\$response->withJson\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Slim $response->withJson sending a JSON response to an inbound request",
                        "This code uses Slim to expose inbound HTTP endpoints returning JSON responses",
                    ),
                ),
                (
                    r"AppFactory::create\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Slim AppFactory::create bootstrapping the Slim application to handle inbound requests",
                        "This code uses Slim AppFactory to expose an inbound HTTP application",
                    ),
                ),
            ],
        },
    },
)
