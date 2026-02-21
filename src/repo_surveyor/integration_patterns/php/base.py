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
                        "PHP cURL initializing an outbound HTTP client request",
                        "This code uses PHP cURL to make outbound HTTP requests",
                    ),
                ),
                (
                    r"curl_setopt\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP cURL configuring an outbound HTTP request option",
                        "This code uses PHP cURL to configure outbound HTTP requests",
                    ),
                ),
                (
                    r"curl_exec\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP cURL executing an outbound HTTP request",
                        "This code uses PHP cURL to send an outbound HTTP request",
                    ),
                ),
                (
                    r"file_get_contents\s*\(\s*['\"]https?://",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP file_get_contents fetching a remote HTTP URL",
                        "This code uses PHP file_get_contents to send an outbound HTTP request",
                    ),
                ),
                (
                    r"new\s+GuzzleHttp\\Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP GuzzleHttp\\Client HTTP client being instantiated",
                        "This code uses PHP Guzzle to send outbound HTTP requests",
                    ),
                ),
                (
                    r"Http::get\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Http::get making an outbound HTTP GET request",
                        "This code uses PHP Http to send an outbound HTTP GET request",
                    ),
                ),
                (
                    r"Http::post\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Http::post making an outbound HTTP POST request",
                        "This code uses PHP Http to send an outbound HTTP POST request",
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
                        "PHP SoapClient being instantiated for outbound SOAP calls",
                        "This code uses PHP SoapClient to call an outbound SOAP web service",
                    ),
                ),
                (
                    r"new\s+SoapServer\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PHP SoapServer being instantiated to handle inbound SOAP requests",
                        "This code uses PHP SoapServer to expose an inbound SOAP web service",
                    ),
                ),
                (
                    r"SoapFault",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP SoapFault handling a SOAP error condition",
                        "This code uses PHP SoapFault to interact with SOAP error handling",
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
                        "PHP PhpAmqpLib AMQP client library usage",
                        "This code uses PHP PhpAmqpLib to interact with an AMQP message broker",
                    ),
                ),
                (
                    r"AMQPConnection",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP AMQPConnection establishing an AMQP broker connection",
                        "This code uses PHP AMQP to interact with an AMQP message broker",
                    ),
                ),
                (
                    r"AMQPChannel",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP AMQPChannel operating on an AMQP messaging channel",
                        "This code uses PHP AMQP to interact with an AMQP messaging channel",
                    ),
                ),
                (
                    r"Enqueue\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP Enqueue message queue abstraction library usage",
                        "This code uses PHP Enqueue to interact with a message queue",
                    ),
                ),
                (
                    r"RdKafka\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP RdKafka Kafka client library usage",
                        "This code uses PHP RdKafka to interact with a Kafka cluster",
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
                        "PHP socket_create creating a raw socket",
                        "This code uses PHP sockets to interact with a network socket",
                    ),
                ),
                (
                    r"socket_connect\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP socket_connect connecting to a remote socket endpoint",
                        "This code uses PHP sockets to connect to a remote socket endpoint",
                    ),
                ),
                (
                    r"socket_listen\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PHP socket_listen binding a socket to accept inbound connections",
                        "This code uses PHP sockets to listen for inbound socket connections",
                    ),
                ),
                (
                    r"stream_socket_server\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PHP stream_socket_server creating an inbound socket server",
                        "This code uses PHP stream sockets to accept inbound socket connections",
                    ),
                ),
                (
                    r"stream_socket_client\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP stream_socket_client creating an outbound stream socket connection",
                        "This code uses PHP stream sockets to connect to a remote socket endpoint",
                    ),
                ),
                (
                    r"Ratchet\\",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PHP Ratchet WebSocket server library usage",
                        "This code uses PHP Ratchet to accept inbound WebSocket connections",
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
                        "PHP PDO database abstraction being instantiated",
                        "This code uses PHP PDO to connect to an outbound database",
                    ),
                ),
                (
                    r"PDO::prepare\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP PDO preparing a parameterized SQL statement",
                        "This code uses PHP PDO to execute outbound database queries",
                    ),
                ),
                (
                    r"mysqli_connect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP mysqli_connect establishing a MySQL database connection",
                        "This code uses PHP mysqli to connect to an outbound MySQL database",
                    ),
                ),
                (
                    r"new\s+mysqli\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP mysqli object being instantiated for MySQL connection",
                        "This code uses PHP mysqli to connect to an outbound MySQL database",
                    ),
                ),
                (
                    r"pg_connect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP pg_connect establishing a PostgreSQL database connection",
                        "This code uses PHP pg to connect to an outbound PostgreSQL database",
                    ),
                ),
                (
                    r"MongoDB\\Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP MongoDB\\Client connecting to a MongoDB database",
                        "This code uses PHP MongoDB to connect to an outbound MongoDB database",
                    ),
                ),
                (
                    r"new\s+Redis\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Redis client being instantiated",
                        "This code uses PHP Redis to connect to an outbound Redis database",
                    ),
                ),
                (
                    r"Doctrine\\ORM",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Doctrine ORM being used for object-relational mapping",
                        "This code uses PHP Doctrine ORM to query an outbound relational database",
                    ),
                ),
                (
                    r"Doctrine\\DBAL",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Doctrine DBAL database abstraction layer usage",
                        "This code uses PHP Doctrine DBAL to execute outbound database queries",
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
                        "PHP fopen opening a file or stream for I/O",
                        "This code uses PHP fopen to interact with a local file or stream",
                    ),
                ),
                (
                    r"fwrite\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "PHP fwrite writing data to a file handle",
                        "This code uses PHP fwrite to write to a local file",
                    ),
                ),
                (
                    r"file_put_contents\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "PHP file_put_contents writing data to a file",
                        "This code uses PHP file_put_contents to write to a local file",
                    ),
                ),
                (
                    r"file_get_contents\s*\(\s*['\"][^h]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "PHP file_get_contents reading data from a local file path",
                        "This code uses PHP file_get_contents to receive data from a local file",
                    ),
                ),
                (
                    r"League\\Flysystem",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP League\\Flysystem filesystem abstraction library usage",
                        "This code uses PHP Flysystem to interact with a local or cloud filesystem",
                    ),
                ),
                (
                    r"Aws\\S3\\",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Aws\\S3 client interacting with AWS S3 storage",
                        "This code uses PHP AWS SDK to write to or read from outbound S3 object storage",
                    ),
                ),
                (
                    r"Storage::disk\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP Storage::disk accessing a configured filesystem disk",
                        "This code uses PHP Storage to interact with a configured file storage backend",
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
                        "PHP Grpc\\ namespace usage for gRPC communication",
                        "This code uses PHP gRPC to interact with a gRPC service",
                    ),
                ),
                (
                    r"GPBMetadata\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP GPBMetadata\\ Protobuf message metadata usage",
                        "This code uses PHP Protobuf to interact with gRPC message definitions",
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
                        "PHP GraphQL\\ library usage for GraphQL API handling",
                        "This code uses PHP GraphQL to interact with a GraphQL API",
                    ),
                ),
                (
                    r"webonyx/graphql-php",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP webonyx/graphql-php library for GraphQL API implementation",
                        "This code uses PHP webonyx/graphql-php to interact with a GraphQL API",
                    ),
                ),
                (
                    r"Lighthouse\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP Lighthouse\\ GraphQL server framework usage",
                        "This code uses PHP Lighthouse to interact with a Laravel GraphQL API",
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
                        "PHP mail() function sending an outbound email",
                        "This code uses PHP mail() to send an outbound email message",
                    ),
                ),
                (
                    r"PHPMailer\\PHPMailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP PHPMailer\\PHPMailer library sending outbound email",
                        "This code uses PHP PHPMailer to send outbound email messages",
                    ),
                ),
                (
                    r"Swift_Mailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Swift_Mailer library sending outbound email",
                        "This code uses PHP SwiftMailer to send outbound email messages",
                    ),
                ),
                (
                    r"Symfony\\.*\\Mailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Symfony Mailer component sending outbound email",
                        "This code uses PHP Symfony Mailer to send outbound email messages",
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
                        "PHP Memcached client connecting to an outbound Memcached cache",
                        "This code uses PHP Memcached to write to or read from an outbound cache",
                    ),
                ),
                (
                    r"new\s+Redis\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP Redis client being instantiated for cache operations",
                        "This code uses PHP Redis to write to or read from an outbound Redis cache",
                    ),
                ),
                (
                    r"apcu_store\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP apcu_store writing a value to the APCu in-process cache",
                        "This code uses PHP APCu to write to an outbound in-process cache",
                    ),
                ),
                (
                    r"apcu_fetch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP apcu_fetch reading a value from the APCu in-process cache",
                        "This code uses PHP APCu to read from an outbound in-process cache",
                    ),
                ),
                (
                    r"Psr\\Cache\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP Psr\\Cache\\ PSR-6 cache interface usage",
                        "This code uses PHP PSR-6 to interact with a cache backend",
                    ),
                ),
                (
                    r"Psr\\SimpleCache\\",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP Psr\\SimpleCache\\ PSR-16 simple cache interface usage",
                        "This code uses PHP PSR-16 to interact with a simple cache backend",
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
                        "PHP text/event-stream content type for SSE communication",
                        "This code uses PHP to interact with a Server-Sent Events stream",
                    ),
                ),
                (
                    r"header\s*\(\s*['\"]Content-Type.*event-stream",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PHP header() setting Content-Type for SSE to stream events to clients",
                        "This code uses PHP to handle inbound Server-Sent Events connections",
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
                        "PHP ftp_connect establishing an outbound FTP connection",
                        "This code uses PHP FTP to connect to an outbound FTP server",
                    ),
                ),
                (
                    r"ftp_login\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP ftp_login authenticating against an outbound FTP server",
                        "This code uses PHP FTP to connect to an outbound FTP server",
                    ),
                ),
                (
                    r"ssh2_sftp\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP ssh2_sftp initializing an outbound SFTP subsystem",
                        "This code uses PHP SSH2 to connect to an outbound SFTP server",
                    ),
                ),
                (
                    r"phpseclib\\Net\\SFTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PHP phpseclib Net\\SFTP client connecting to an SFTP server",
                        "This code uses PHP phpseclib to connect to an outbound SFTP server",
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
                        "PHP cron_schedule registering a cron-based scheduled task",
                        "This code uses PHP to interact with scheduled cron tasks",
                    ),
                ),
                (
                    r"->cron\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP scheduler cron expression defining a recurring task schedule",
                        "This code uses PHP to interact with a cron-expression scheduled task",
                    ),
                ),
                (
                    r"->everyMinute\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP scheduler everyMinute scheduling a task to run every minute",
                        "This code uses PHP to interact with a minutely scheduled task",
                    ),
                ),
                (
                    r"->daily\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "PHP scheduler daily scheduling a task to run once per day",
                        "This code uses PHP to interact with a daily scheduled task",
                    ),
                ),
            ],
        },
    },
)
