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
                        "Angular HttpClientModule import for outbound HTTP requests",
                        "This code uses Angular HttpClient to send outbound HTTP requests",
                    ),
                ),
                (
                    r"HttpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Angular HttpClient class sending outbound HTTP requests",
                        "This code uses Angular HttpClient to send outbound HTTP requests",
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
                        "Angular WebSocket module import for outbound WebSocket connections",
                        "This code uses Angular to connect to an outbound WebSocket endpoint",
                    ),
                ),
            ],
        },
    },
)
