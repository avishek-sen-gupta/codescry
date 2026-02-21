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
                        "Qt QSqlDatabase class opening a database connection",
                        "This code uses Qt to connect to a relational database",
                    ),
                ),
                (
                    r"\bQSqlQuery\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Qt QSqlQuery class executing a SQL query",
                        "This code uses Qt to query a relational database",
                    ),
                ),
                (
                    r"\bQSqlTableModel\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Qt QSqlTableModel class providing a table-based database model",
                        "This code uses Qt to interact with a relational database table",
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
                        "Qt QNetworkAccessManager class sending outbound HTTP requests",
                        "This code uses Qt to send outbound HTTP requests",
                    ),
                ),
                (
                    r"\bQNetworkReply\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Qt QNetworkReply class handling HTTP response data",
                        "This code uses Qt to send outbound HTTP requests",
                    ),
                ),
                (
                    r"\bQNetworkRequest\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Qt QNetworkRequest class constructing an outbound HTTP request",
                        "This code uses Qt to send outbound HTTP requests",
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
                        "Qt QTcpSocket class for TCP socket communication",
                        "This code uses Qt to interact with a TCP socket",
                    ),
                ),
                (
                    r"\bQUdpSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Qt QUdpSocket class for UDP socket communication",
                        "This code uses Qt to interact with a UDP socket",
                    ),
                ),
                (
                    r"\bQTcpServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Qt QTcpServer class accepting inbound TCP connections",
                        "This code uses Qt to accept inbound TCP connections",
                    ),
                ),
                (
                    r"\bQWebSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Qt QWebSocket class for WebSocket communication",
                        "This code uses Qt to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"\bQWebSocketServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Qt QWebSocketServer class accepting inbound WebSocket connections",
                        "This code uses Qt to accept inbound WebSocket connections",
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
                        "Qt QFile class for reading and writing files",
                        "This code uses Qt to interact with a file",
                    ),
                ),
                (
                    r"\bQDir\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Qt QDir class for directory navigation and file listing",
                        "This code uses Qt to interact with the file system",
                    ),
                ),
                (
                    r"\bQFileInfo\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Qt QFileInfo class for querying file metadata",
                        "This code uses Qt to interact with file system metadata",
                    ),
                ),
                (
                    r"\bQTextStream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Qt QTextStream class for reading and writing text files",
                        "This code uses Qt to interact with a text file stream",
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
                        "Qt QTimer class for scheduling periodic or one-shot callbacks",
                        "This code uses Qt to interact with scheduled timer tasks",
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
                        "Qt QWebSocket class for streaming data over WebSocket",
                        "This code uses Qt to interact with a WebSocket streaming connection",
                    ),
                ),
            ],
        },
    },
)
