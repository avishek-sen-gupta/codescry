"""Express framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Express",
    import_patterns=(r"require\(['\"]express['\"]\)",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]express['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Node.js Express framework require for HTTP server",
                        "This code uses Node.js Express for inbound HTTP server integration",
                    ),
                ),
                (
                    r"\w*\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Express router .get handler for HTTP GET route",
                        "This code uses Node.js Express to handle inbound HTTP GET requests",
                    ),
                ),
                (
                    r"\w*\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Express router .post handler for HTTP POST route",
                        "This code uses Node.js Express to handle inbound HTTP POST requests",
                    ),
                ),
                (
                    r"\w*\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Express router .put handler for HTTP PUT route",
                        "This code uses Node.js Express to handle inbound HTTP PUT requests",
                    ),
                ),
                (
                    r"\w*\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Express router .delete handler for HTTP DELETE route",
                        "This code uses Node.js Express to handle inbound HTTP DELETE requests",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"express-graphql",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Express express-graphql middleware for GraphQL server",
                        "This code uses Node.js Express to expose inbound GraphQL APIs",
                    ),
                ),
            ],
        },
    },
)
