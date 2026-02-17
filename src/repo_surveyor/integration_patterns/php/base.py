"""PHP base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"curl_init\(", Confidence.HIGH),
                (r"curl_setopt\(", Confidence.HIGH),
                (r"curl_exec\(", Confidence.HIGH),
                (r"file_get_contents\s*\(\s*['\"]https?://", Confidence.HIGH),
                (r"new\s+GuzzleHttp\\Client", Confidence.HIGH),
                (r"Http::get\(", Confidence.MEDIUM),
                (r"Http::post\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"new\s+SoapClient\(", Confidence.HIGH),
                (r"new\s+SoapServer\(", Confidence.HIGH),
                (r"SoapFault", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"PhpAmqpLib\\", Confidence.HIGH),
                (r"AMQPConnection", Confidence.HIGH),
                (r"AMQPChannel", Confidence.HIGH),
                (r"Enqueue\\", Confidence.HIGH),
                (r"RdKafka\\", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"socket_create\(", Confidence.HIGH),
                (r"socket_connect\(", Confidence.HIGH),
                (r"socket_listen\(", Confidence.HIGH),
                (r"stream_socket_server\(", Confidence.HIGH),
                (r"stream_socket_client\(", Confidence.HIGH),
                (r"Ratchet\\", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"new\s+PDO\(", Confidence.HIGH),
                (r"PDO::prepare\(", Confidence.HIGH),
                (r"mysqli_connect\(", Confidence.HIGH),
                (r"new\s+mysqli\(", Confidence.HIGH),
                (r"pg_connect\(", Confidence.HIGH),
                (r"MongoDB\\Client", Confidence.HIGH),
                (r"new\s+Redis\b", Confidence.HIGH),
                (r"Doctrine\\ORM", Confidence.HIGH),
                (r"Doctrine\\DBAL", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"fopen\(", Confidence.MEDIUM),
                (r"fwrite\(", Confidence.MEDIUM),
                (r"file_put_contents\(", Confidence.MEDIUM),
                (r"file_get_contents\s*\(\s*['\"][^h]", Confidence.MEDIUM),
                (r"League\\Flysystem", Confidence.HIGH),
                (r"Aws\\S3\\", Confidence.HIGH),
                (r"Storage::disk\(", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"Grpc\\", Confidence.HIGH),
                (r"GPBMetadata\\", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"GraphQL\\", Confidence.HIGH),
                (r"webonyx/graphql-php", Confidence.HIGH),
                (r"Lighthouse\\", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"mail\s*\(", Confidence.MEDIUM),
                (r"PHPMailer\\PHPMailer", Confidence.HIGH),
                (r"Swift_Mailer", Confidence.HIGH),
                (r"Symfony\\.*\\Mailer", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"Memcached\b", Confidence.HIGH),
                (r"new\s+Redis\b", Confidence.HIGH),
                (r"apcu_store\(", Confidence.HIGH),
                (r"apcu_fetch\(", Confidence.HIGH),
                (r"Psr\\Cache\\", Confidence.HIGH),
                (r"Psr\\SimpleCache\\", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"text/event-stream", Confidence.MEDIUM),
                (r"header\s*\(\s*['\"]Content-Type.*event-stream", Confidence.HIGH),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"ftp_connect\(", Confidence.HIGH),
                (r"ftp_login\(", Confidence.HIGH),
                (r"ssh2_sftp\(", Confidence.HIGH),
                (r"phpseclib\\Net\\SFTP", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"cron_schedule\(", Confidence.HIGH),
                (r"->cron\(", Confidence.MEDIUM),
                (r"->everyMinute\(", Confidence.HIGH),
                (r"->daily\(", Confidence.HIGH),
            ],
        },
    },
)
