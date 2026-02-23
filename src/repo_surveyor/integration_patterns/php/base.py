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
                (
                    r"curl_init\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is initialized via cURL",
                        "HTTP request is made with client",
                    ),
                ),
                (
                    r"curl_setopt\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP request option is configured via cURL",
                        "HTTP request is configured with options",
                    ),
                ),
                (
                    r"curl_exec\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP request is executed via cURL",
                        "HTTP request is sent with parameters",
                    ),
                ),
                (
                    r"file_get_contents\s*\(\s*['\"]https?://",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP URL is fetched remotely",
                        "HTTP request is sent with file functions",
                    ),
                ),
                (
                    r"new\s+GuzzleHttp\\Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is created for outbound requests",
                        "HTTP request is sent with client",
                    ),
                ),
                (
                    r"Http::get\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP GET request is made outbound",
                        "HTTP GET request is sent to endpoint",
                    ),
                ),
                (
                    r"Http::post\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP POST request is made outbound",
                        "HTTP POST request is sent to endpoint",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"new\s+SoapClient\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP client is created for outbound calls",
                        "SOAP service is called with client request",
                    ),
                ),
                (
                    r"new\s+SoapServer\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP server is created for inbound requests",
                        "SOAP service is exposed for inbound requests",
                    ),
                ),
                (
                    r"SoapFault",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP error is handled via fault",
                        "SOAP errors are handled with fault processing",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"PhpAmqpLib\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP client is accessed for messaging",
                        "AMQP broker is accessed for message exchange",
                    ),
                ),
                (
                    r"AMQPConnection",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP connection is established with broker",
                        "AMQP broker is accessed for message exchange",
                    ),
                ),
                (
                    r"AMQPChannel",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP channel is operated for messaging",
                        "AMQP channel is accessed for messaging",
                    ),
                ),
                (
                    r"Enqueue\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Message queue is accessed via abstraction library",
                        "Message queue is accessed for processing",
                    ),
                ),
                (
                    r"RdKafka\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kafka client is accessed for messaging",
                        "Kafka cluster is accessed for message streaming",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"socket_create\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Raw socket is created",
                        "Network socket is accessed for communication",
                    ),
                ),
                (
                    r"socket_connect\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Socket endpoint is connected remotely",
                        "Socket endpoint is connected to remotely",
                    ),
                ),
                (
                    r"socket_listen\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket is bound for inbound connections",
                        "Socket connections are listened for inbound traffic",
                    ),
                ),
                (
                    r"stream_socket_server\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket server is created for inbound connections",
                        "Socket connections are accepted for inbound traffic",
                    ),
                ),
                (
                    r"stream_socket_client\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Stream socket is connected outbound",
                        "Socket endpoint is connected to remotely",
                    ),
                ),
                (
                    r"Ratchet\\",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket server is created with library",
                        "WebSocket connections are accepted for inbound communication",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"new\s+PDO\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database abstraction is created via PDO",
                        "Database is connected to with credentials",
                    ),
                ),
                (
                    r"PDO::prepare\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL statement is prepared with parameters",
                        "Database queries are executed with prepared statements",
                    ),
                ),
                (
                    r"mysqli_connect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL database is connected via mysqli",
                        "MySQL database is connected to with driver",
                    ),
                ),
                (
                    r"new\s+mysqli\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL connection is created via mysqli",
                        "MySQL database is connected to with driver",
                    ),
                ),
                (
                    r"pg_connect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL database is connected via pg_connect",
                        "PostgreSQL database is connected to with driver",
                    ),
                ),
                (
                    r"MongoDB\\Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB database is connected via client",
                        "MongoDB database is connected to with driver",
                    ),
                ),
                (
                    r"new\s+Redis\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis client is created for cache",
                        "Redis database is connected to with driver",
                    ),
                ),
                (
                    r"Doctrine\\ORM",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Object-relational mapping is handled by ORM",
                        "Database is queried with ORM mapping",
                    ),
                ),
                (
                    r"Doctrine\\DBAL",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database abstraction is accessed via DBAL",
                        "Database queries are executed with connection",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"fopen\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File stream is opened for I/O",
                        "File stream is opened for operations",
                    ),
                ),
                (
                    r"fwrite\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "File data is written to handle",
                        "Local file is written with handle",
                    ),
                ),
                (
                    r"file_put_contents\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "File data is written to path",
                        "Local file is written with content",
                    ),
                ),
                (
                    r"file_get_contents\s*\(\s*['\"][^h]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "File data is read from local path",
                        "Local file is read with stream",
                    ),
                ),
                (
                    r"League\\Flysystem",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem is accessed via abstraction library",
                        "Filesystem is accessed for file operations",
                    ),
                ),
                (
                    r"Aws\\S3\\",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 client is created for AWS storage interaction",
                        "S3 storage is accessed for object operations",
                    ),
                ),
                (
                    r"Storage::disk\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem disk is accessed via storage",
                        "File storage is accessed with backend configuration",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"Grpc\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "gRPC communication is accessed via namespace",
                        "gRPC service is accessed with client",
                    ),
                ),
                (
                    r"GPBMetadata\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Protobuf metadata is accessed for message handling",
                        "gRPC messages are processed with protobuf definitions",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"GraphQL\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL API is handled by library",
                        "GraphQL API is accessed with queries",
                    ),
                ),
                (
                    r"webonyx/graphql-php",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL API is implemented via library",
                        "GraphQL API is accessed via webonyx client",
                    ),
                ),
                (
                    r"Lighthouse\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL server is created with framework",
                        "GraphQL API is accessed with Laravel schema",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"mail\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent outbound via mail function",
                        "Email message is sent with mail function",
                    ),
                ),
                (
                    r"PHPMailer\\PHPMailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent outbound via PHPMailer",
                        "Email message is sent via SMTP",
                    ),
                ),
                (
                    r"Swift_Mailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent outbound via Swift Mailer",
                        "Email message is sent via transport",
                    ),
                ),
                (
                    r"Symfony\\.*\\Mailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent outbound via Symfony Mailer",
                        "Email message is sent via mailer",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"Memcached\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached cache is connected for outbound access",
                        "Cache is accessed for read write operations",
                    ),
                ),
                (
                    r"new\s+Redis\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis client is created for cache operations",
                        "Redis cache is accessed for data operations",
                    ),
                ),
                (
                    r"apcu_store\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache value is written to APCu",
                        "Cache is written to in-process storage",
                    ),
                ),
                (
                    r"apcu_fetch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache value is read from APCu",
                        "Cache is read from in-process storage",
                    ),
                ),
                (
                    r"Psr\\Cache\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache is accessed via PSR-6 interface",
                        "Cache backend is accessed with pool interface",
                    ),
                ),
                (
                    r"Psr\\SimpleCache\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache is accessed via PSR-16 interface",
                        "Cache backend is accessed with simple interface",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SSE communication is indicated via content type",
                        "Server-Sent Events stream is accessed",
                    ),
                ),
                (
                    r"header\s*\(\s*['\"]Content-Type.*event-stream",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SSE events are streamed via Content-Type header",
                        "Server-Sent Events connection is handled for inbound streams",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"ftp_connect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP connection is established outbound",
                        "FTP server is connected to with credentials",
                    ),
                ),
                (
                    r"ftp_login\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP server is authenticated outbound",
                        "FTP server is connected to with credentials",
                    ),
                ),
                (
                    r"ssh2_sftp\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP subsystem is initialized outbound",
                        "SFTP server is connected to with credentials",
                    ),
                ),
                (
                    r"phpseclib\\Net\\SFTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP server is connected via phpseclib",
                        "SFTP server is connected to with credentials",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"cron_schedule\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cron task is registered with schedule",
                        "Cron tasks are scheduled",
                    ),
                ),
                (
                    r"->cron\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Task schedule is defined with cron expression",
                        "Cron expression task is scheduled",
                    ),
                ),
                (
                    r"->everyMinute\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Minute task is scheduled via scheduler",
                        "Minutely task is scheduled",
                    ),
                ),
                (
                    r"->daily\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Daily task is scheduled via scheduler",
                        "Daily task is scheduled",
                    ),
                ),
            ],
        },
    },
)
