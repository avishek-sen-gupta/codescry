"""PHP base integration patterns."""

from repo_surveyor.integration_patterns.types import (
    BasePatternSpec,
    Confidence,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"curl_init\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"curl_setopt\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"curl_exec\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"file_get_contents\s*\(\s*['\"]https?://",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"new\s+GuzzleHttp\\Client", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Http::get\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"Http::post\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"new\s+SoapClient\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"new\s+SoapServer\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"SoapFault", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"PhpAmqpLib\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"AMQPConnection", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"AMQPChannel", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Enqueue\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"RdKafka\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"socket_create\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"socket_connect\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"socket_listen\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"stream_socket_server\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"stream_socket_client\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Ratchet\\", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"new\s+PDO\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PDO::prepare\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"mysqli_connect\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"new\s+mysqli\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"pg_connect\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MongoDB\\Client", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"new\s+Redis\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Doctrine\\ORM", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Doctrine\\DBAL", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"fopen\(", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"fwrite\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"file_put_contents\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (
                    r"file_get_contents\s*\(\s*['\"][^h]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                ),
                (r"League\\Flysystem", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Aws\\S3\\", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Storage::disk\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"Grpc\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"GPBMetadata\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"GraphQL\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"webonyx/graphql-php", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Lighthouse\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"mail\s*\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"PHPMailer\\PHPMailer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Swift_Mailer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Symfony\\.*\\Mailer", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Memcached\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"new\s+Redis\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"apcu_store\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"apcu_fetch\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Psr\\Cache\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Psr\\SimpleCache\\", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"text/event-stream", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (
                    r"header\s*\(\s*['\"]Content-Type.*event-stream",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"ftp_connect\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ftp_login\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ssh2_sftp\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"phpseclib\\Net\\SFTP", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"cron_schedule\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"->cron\(", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"->everyMinute\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"->daily\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
