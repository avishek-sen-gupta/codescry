"""POCO framework integration patterns."""

from ..types import (
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
                (r"\bHTTPServer\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bHTTPClientSession\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bHTTPRequestHandler\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\bServerSocket\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bStreamSocket\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"Poco::Data::Session", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Poco::Data::Statement", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Poco::Data::ODBC", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"\bMailMessage\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bSMTPClientSession\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"\bFTPClientSession\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bSFTPClientSession\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"Poco::File\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"Poco::DirectoryIterator\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
    },
)
