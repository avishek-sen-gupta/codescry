"""POCO framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="POCO",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\bHTTPServer\b", Confidence.HIGH),
                (r"\bHTTPClientSession\b", Confidence.HIGH),
                (r"\bHTTPRequestHandler\b", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"\bServerSocket\b", Confidence.HIGH),
                (r"\bStreamSocket\b", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"Poco::Data::Session", Confidence.HIGH),
                (r"Poco::Data::Statement", Confidence.HIGH),
                (r"Poco::Data::ODBC", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"\bMailMessage\b", Confidence.HIGH),
                (r"\bSMTPClientSession\b", Confidence.HIGH),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"\bFTPClientSession\b", Confidence.HIGH),
                (r"\bSFTPClientSession\b", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"Poco::File\b", Confidence.HIGH),
                (r"Poco::DirectoryIterator\b", Confidence.HIGH),
            ],
        },
    },
)
