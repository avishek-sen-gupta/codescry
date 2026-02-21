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
                        "Sanic framework import for building async REST API applications",
                        "This code uses Sanic to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"Sanic\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Sanic application instantiation for configuring the async API server",
                        "This code uses Sanic to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"@\w+\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Sanic @app.get decorator defining an inbound GET REST endpoint",
                        "This code uses Sanic to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@\w+\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Sanic @app.post decorator defining an inbound POST REST endpoint",
                        "This code uses Sanic to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@\w+\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Sanic @app.put decorator defining an inbound PUT REST endpoint",
                        "This code uses Sanic to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@\w+\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Sanic @app.delete decorator defining an inbound DELETE REST endpoint",
                        "This code uses Sanic to expose an inbound REST API endpoint",
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
                        "Sanic @app.websocket decorator defining an inbound WebSocket handler",
                        "This code uses Sanic to handle incoming WebSocket connection requests",
                    ),
                ),
            ],
        },
    },
)
