"""POCO framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="POCO",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"\bHTTPServer\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is created for inbound connections",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"\bHTTPClientSession\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP request is sent to remote endpoint",
                        "HTTP request is sent to outbound endpoint",
                    ),
                ),
                (
                    r"\bHTTPRequestHandler\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request is handled by inbound handler",
                        "HTTP request is handled by server",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"\bServerSocket\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket connections are accepted for inbound traffic",
                        "Socket connection is accepted for inbound requests",
                    ),
                ),
                (
                    r"\bStreamSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP stream is communicated via socket",
                        "TCP socket stream is accessed for communication",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"Poco::Data::Session",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database session is opened by Session class",
                        "Database connection is opened with credentials",
                    ),
                ),
                (
                    r"Poco::Data::Statement",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database statement is executed by SQL command",
                        "Database is queried with SQL",
                    ),
                ),
                (
                    r"Poco::Data::ODBC",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ODBC database is accessed by Data connector",
                        "Database connection is opened via ODBC",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"\bMailMessage\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is composed for outbound delivery",
                        "Email message is sent via outbound service",
                    ),
                ),
                (
                    r"\bSMTPClientSession\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent via SMTP protocol",
                        "Email is sent via SMTP protocol",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"\bFTPClientSession\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP connection is opened to remote server",
                        "FTP connection is opened for outbound transfer",
                    ),
                ),
                (
                    r"\bSFTPClientSession\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP connection is opened to secure server",
                        "SFTP connection is opened for outbound transfer",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"Poco::File\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File operations are performed on filesystem",
                        "File system is accessed for operations",
                    ),
                ),
                (
                    r"Poco::DirectoryIterator\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Directory entries are iterated by file system traversal",
                        "File system is accessed for operations",
                    ),
                ),
            ],
        },
    },
)
