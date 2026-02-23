"""Angular framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Angular",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@angular/common/http['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP module is imported for outbound requests",
                        "HTTP request is sent via client",
                    ),
                ),
                (
                    r"HttpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP requests are sent with client",
                        "HTTP request is sent via client",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@angular/.*websocket",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "WebSocket connections are imported for outbound",
                        "WebSocket endpoint is connected for outbound communication",
                    ),
                ),
            ],
        },
    },
)
