"""Play framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Play",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import play\.api\.mvc",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play framework mvc import for HTTP controller support",
                        "This code uses Play to handle inbound HTTP MVC requests",
                    ),
                ),
                (
                    r"Action\s*\{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play framework Action block for HTTP request handling",
                        "This code uses Play to handle inbound HTTP action requests",
                    ),
                ),
                (
                    r"Action\.async\s*\{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play framework Action.async block for async HTTP request handling",
                        "This code uses Play to handle inbound HTTP requests asynchronously",
                    ),
                ),
                (
                    r"Ok\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Play framework Ok response for HTTP 200 reply",
                        "This code uses Play to handle inbound HTTP requests and respond",
                    ),
                ),
                (
                    r"Json\.toJson\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play framework Json.toJson for JSON response serialization",
                        "This code uses Play to handle inbound HTTP requests with JSON responses",
                    ),
                ),
                (
                    r"def\s+\w+.*=\s*Action",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play framework Action method definition for HTTP endpoint",
                        "This code uses Play to expose inbound HTTP endpoint actions",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"import play\.api\.db",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Play framework db import for database access",
                        "This code uses Play to query a relational database",
                    ),
                ),
                (
                    r"import anorm\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Play framework Anorm import for SQL database access",
                        "This code uses Play Anorm to query a relational database",
                    ),
                ),
                (
                    r"SQL\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Play framework Anorm SQL call for database query execution",
                        "This code uses Play Anorm to query a relational database with SQL",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"import play\.api\.libs\.streams",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play framework streams import for WebSocket and streaming support",
                        "This code uses Play to handle inbound WebSocket connections",
                    ),
                ),
                (
                    r"WebSocket\.accept",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play framework WebSocket.accept for WebSocket connection acceptance",
                        "This code uses Play to accept inbound WebSocket connections",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"import play\.api\.cache",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Play framework cache import for cache integration",
                        "This code uses Play to interact with a cache layer",
                    ),
                ),
                (
                    r"@Cached",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Play framework @Cached annotation for action caching",
                        "This code uses Play to interact with cached HTTP action responses",
                    ),
                ),
            ],
        },
    },
)
