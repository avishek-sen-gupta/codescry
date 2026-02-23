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
                (
                    r"\bQSqlDatabase\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened via SQL database",
                        "Database is connected via Qt driver",
                    ),
                ),
                (
                    r"\bQSqlQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed on database",
                        "Database is queried via Qt",
                    ),
                ),
                (
                    r"\bQSqlTableModel\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is modeled with SQL interface",
                        "Database table is accessed via Qt",
                    ),
                ),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"\bQNetworkAccessManager\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP request is sent via network manager",
                        "HTTP requests are sent via Qt",
                    ),
                ),
                (
                    r"\bQNetworkReply\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP response is handled by reply class",
                        "HTTP requests are sent via Qt",
                    ),
                ),
                (
                    r"\bQNetworkRequest\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP request is constructed for outbound call",
                        "HTTP requests are sent via Qt",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"\bQTcpSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP socket is created for network communication",
                        "TCP socket is opened for communication",
                    ),
                ),
                (
                    r"\bQUdpSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP socket is created for datagram communication",
                        "UDP socket is accessed via Qt",
                    ),
                ),
                (
                    r"\bQTcpServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TCP connections are accepted by server",
                        "TCP connections are accepted via Qt",
                    ),
                ),
                (
                    r"\bQWebSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WebSocket is created for bidirectional communication",
                        "WebSocket connection is accessed via Qt",
                    ),
                ),
                (
                    r"\bQWebSocketServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connections are accepted by server",
                        "WebSocket connections are accepted via Qt",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"\bQFile\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File is accessed for read-write operations",
                        "File is accessed via Qt",
                    ),
                ),
                (
                    r"\bQDir\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Directory is navigated with file listing",
                        "File system is accessed via Qt",
                    ),
                ),
                (
                    r"\bQFileInfo\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File metadata is queried via info class",
                        "File system metadata is accessed via Qt",
                    ),
                ),
                (
                    r"\bQTextStream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Text file is accessed via stream interface",
                        "Text file stream is accessed via Qt",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"\bQTimer\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Timer is scheduled for periodic callbacks",
                        "Timer tasks are scheduled via Qt",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"\bQWebSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Data is streamed over WebSocket connection",
                        "WebSocket streaming connection is accessed via Qt",
                    ),
                ),
            ],
        },
    },
)
