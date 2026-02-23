"""Sanic framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Sanic",
    import_patterns=(r"from sanic import", r"import sanic"),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from sanic import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Async REST API is built with framework import",
                        "HTTP endpoint is accessed via Sanic",
                    ),
                ),
                (
                    r"Sanic\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Async API server is created for configuration",
                        "HTTP endpoint is accessed via Sanic",
                    ),
                ),
                (
                    r"@\w+\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is registered for inbound requests",
                        "REST endpoint is exposed via Sanic",
                    ),
                ),
                (
                    r"@\w+\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is registered for inbound requests",
                        "REST endpoint is exposed via Sanic",
                    ),
                ),
                (
                    r"@\w+\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT route is registered for inbound requests",
                        "REST endpoint is exposed via Sanic",
                    ),
                ),
                (
                    r"@\w+\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE route is registered for inbound requests",
                        "REST endpoint is exposed via Sanic",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"@\w+\.websocket\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket handler is registered for inbound connections",
                        "WebSocket connection is handled via Sanic",
                    ),
                ),
            ],
        },
    },
)
