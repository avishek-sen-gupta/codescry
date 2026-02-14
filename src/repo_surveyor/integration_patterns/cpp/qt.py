"""Qt framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Qt",
    patterns={
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"\bQSqlDatabase\b", Confidence.HIGH),
                (r"\bQSqlQuery\b", Confidence.HIGH),
                (r"\bQSqlTableModel\b", Confidence.HIGH),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\bQNetworkAccessManager\b", Confidence.HIGH),
                (r"\bQNetworkReply\b", Confidence.HIGH),
                (r"\bQNetworkRequest\b", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\bQTcpSocket\b", Confidence.HIGH),
                (r"\bQUdpSocket\b", Confidence.HIGH),
                (r"\bQTcpServer\b", Confidence.HIGH),
                (r"\bQWebSocket\b", Confidence.HIGH),
                (r"\bQWebSocketServer\b", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"\bQFile\b", Confidence.HIGH),
                (r"\bQDir\b", Confidence.HIGH),
                (r"\bQFileInfo\b", Confidence.HIGH),
                (r"\bQTextStream\b", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\bQTimer\b", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"\bQWebSocket\b", Confidence.HIGH),
            ],
        },
    },
)
