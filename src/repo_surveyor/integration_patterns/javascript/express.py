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
                        "HTTP server is imported via Express",
                        "HTTP server is exposed for inbound integration",
                    ),
                ),
                (
                    r"\w*\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is handled by router",
                        "HTTP GET request is handled for inbound",
                    ),
                ),
                (
                    r"\w*\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is handled by router",
                        "HTTP POST request is handled for inbound",
                    ),
                ),
                (
                    r"\w*\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT route is handled by router",
                        "HTTP PUT request is handled for inbound",
                    ),
                ),
                (
                    r"\w*\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE route is handled by router",
                        "HTTP DELETE request is handled for inbound",
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
                        "GraphQL server is enabled via middleware",
                        "GraphQL API is exposed for inbound requests",
                    ),
                ),
            ],
        },
    },
)
