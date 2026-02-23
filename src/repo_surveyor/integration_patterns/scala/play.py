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
                        "MVC controller is imported for HTTP handling",
                        "HTTP request is handled by MVC controller",
                    ),
                ),
                (
                    r"Action\s*\{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request is handled by action block",
                        "HTTP action is handled for inbound requests",
                    ),
                ),
                (
                    r"Action\.async\s*\{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request is handled by async action",
                        "HTTP request is handled asynchronously",
                    ),
                ),
                (
                    r"Ok\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is returned with OK status",
                        "HTTP request is handled with response",
                    ),
                ),
                (
                    r"Json\.toJson\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JSON response is serialized for output",
                        "HTTP request is handled with JSON response",
                    ),
                ),
                (
                    r"def\s+\w+.*=\s*Action",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP endpoint is defined by action method",
                        "HTTP action is exposed for inbound requests",
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
                        "Database access is imported for persistence",
                        "Database is queried with SQL",
                    ),
                ),
                (
                    r"import anorm\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database access is imported for SQL operations",
                        "Database is queried with SQL",
                    ),
                ),
                (
                    r"SQL\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database query is executed via SQL call",
                        "Database is queried with SQL",
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
                        "Streaming support is imported for WebSocket communication",
                        "WebSocket connection is handled for inbound requests",
                    ),
                ),
                (
                    r"WebSocket\.accept",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connection is accepted for bidirectional communication",
                        "WebSocket connection is accepted for inbound requests",
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
                        "Cache integration is imported for performance",
                        "Cache layer is accessed for storage",
                    ),
                ),
                (
                    r"@Cached",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Action is annotated for caching behavior",
                        "HTTP response is cached by action",
                    ),
                ),
            ],
        },
    },
)
