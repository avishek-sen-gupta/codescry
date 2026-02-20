"""Qt framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Qt",
    patterns={
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"\bQSqlDatabase\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bQSqlQuery\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bQSqlTableModel\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"\bQNetworkAccessManager\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"\bQNetworkReply\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bQNetworkRequest\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\bQTcpSocket\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bQUdpSocket\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bQTcpServer\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bQWebSocket\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bQWebSocketServer\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"\bQFile\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bQDir\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bQFileInfo\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bQTextStream\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\bQTimer\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"\bQWebSocket\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
