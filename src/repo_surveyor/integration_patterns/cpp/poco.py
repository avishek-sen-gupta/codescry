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
                        "POCO HTTPServer class creating an inbound HTTP server",
                        "This code uses POCO to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"\bHTTPClientSession\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "POCO HTTPClientSession class making outbound HTTP requests",
                        "This code uses POCO to send outbound HTTP requests",
                    ),
                ),
                (
                    r"\bHTTPRequestHandler\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POCO HTTPRequestHandler class handling inbound HTTP requests",
                        "This code uses POCO to handle inbound HTTP requests",
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
                        "POCO ServerSocket class accepting inbound socket connections",
                        "This code uses POCO to accept inbound socket connections",
                    ),
                ),
                (
                    r"\bStreamSocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "POCO StreamSocket class for TCP stream communication",
                        "This code uses POCO to interact with a TCP stream socket",
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
                        "POCO Data::Session class opening a database session",
                        "This code uses POCO Data to connect to a relational database",
                    ),
                ),
                (
                    r"Poco::Data::Statement",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "POCO Data::Statement class executing a database statement",
                        "This code uses POCO Data to query a relational database",
                    ),
                ),
                (
                    r"Poco::Data::ODBC",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "POCO Data::ODBC connector for ODBC database access",
                        "This code uses POCO Data ODBC to connect to a relational database",
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
                        "POCO MailMessage class composing an outbound email message",
                        "This code uses POCO to send outbound email messages",
                    ),
                ),
                (
                    r"\bSMTPClientSession\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "POCO SMTPClientSession class sending email via SMTP",
                        "This code uses POCO to send outbound email via SMTP",
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
                        "POCO FTPClientSession class connecting to an FTP server",
                        "This code uses POCO to connect to an outbound FTP server",
                    ),
                ),
                (
                    r"\bSFTPClientSession\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "POCO SFTPClientSession class connecting to an SFTP server",
                        "This code uses POCO to connect to an outbound SFTP server",
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
                        "POCO File class for file system operations",
                        "This code uses POCO to interact with the file system",
                    ),
                ),
                (
                    r"Poco::DirectoryIterator\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "POCO DirectoryIterator class iterating over directory entries",
                        "This code uses POCO to interact with the file system",
                    ),
                ),
            ],
        },
    },
)
