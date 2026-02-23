"""Common integration patterns that apply across all languages."""

from repo_surveyor.integration_patterns.types import (
    BasePatternSpec,
    Confidence,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

COMMON = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bhttp\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP communication is indicated",
                        "HTTP web-based integration is referenced",
                    ),
                ),
                (
                    r"(?i)\brest(?:ful)?\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "REST API integration is indicated",
                        "REST HTTP API communication is referenced",
                    ),
                ),
                (
                    r"(?i)\bapi\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "API integration is indicated",
                        "API communication is referenced for external services",
                    ),
                ),
                (
                    r"(?i)\bendpoint\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Network endpoint is indicated",
                        "Service endpoint is referenced for URL exposure",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^controllers?$",
                r"(?i)^api$",
                r"(?i)^rest$",
                r"(?i)^endpoints?$",
                r"(?i)^resources?$",
                r"(?i)^handlers?$",
                r"(?i)^routes?$",
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bsoap\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP web service communication is indicated",
                        "SOAP XML web service is referenced",
                    ),
                ),
                (
                    r"(?i)\bwsdl\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP service is described by WSDL",
                        "WSDL descriptor is referenced for SOAP service interface",
                    ),
                ),
                (
                    r"(?i)\benvelope\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML message is wrapped in SOAP envelope",
                        "SOAP envelope is referenced for XML message structure",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^soap$",
                r"(?i)^wsdl$",
                r"(?i)^webservices?$",
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bkafka\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Event streaming is indicated via Kafka",
                        "Kafka message broker is referenced",
                    ),
                ),
                (
                    r"(?i)\brabbit(?:mq)?\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP message broker is indicated",
                        "RabbitMQ message queue is referenced",
                    ),
                ),
                (
                    r"(?i)\bamqp\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP communication is indicated",
                        "AMQP message queue protocol is referenced",
                    ),
                ),
                (
                    r"(?i)\bpulsar\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Distributed messaging is indicated via Pulsar",
                        "Pulsar distributed messaging is referenced",
                    ),
                ),
                (
                    r"(?i)\bnats\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Lightweight messaging is indicated by NATS",
                        "NATS pub-sub messaging is referenced",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^messaging$",
                r"(?i)^kafka$",
                r"(?i)^rabbit(?:mq)?$",
                r"(?i)^queues?$",
                r"(?i)^events?$",
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bwebsocket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WebSocket communication is indicated",
                        "WebSocket connection is referenced for bidirectional communication",
                    ),
                ),
                (
                    r"(?i)\btcp\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP network communication is indicated",
                        "TCP socket communication is referenced",
                    ),
                ),
                (
                    r"(?i)\budp\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP communication is indicated",
                        "UDP communication is referenced for connectionless datagrams",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^sockets?$",
                r"(?i)^websockets?$",
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bdatabase\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Database persistence is indicated",
                        "Database integration is referenced for data storage",
                    ),
                ),
                (
                    r"(?i)\bdatasource\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Database connection configuration is indicated by Datasource keyword",
                        "Datasource connection is referenced for database access",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^repositor(?:y|ies)$",
                r"(?i)^dao$",
                r"(?i)^database$",
                r"(?i)^db$",
                r"(?i)^persistence$",
                r"(?i)^models?$",
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bfile\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem interaction is indicated",
                        "File operations are referenced for filesystem integration",
                    ),
                ),
                (
                    r"(?i)\bupload\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File upload is indicated",
                        "File upload is referenced for inbound transfer",
                    ),
                ),
                (
                    r"(?i)\bdownload\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File is retrieved from server",
                        "File download is referenced for outbound retrieval",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^uploads?$",
                r"(?i)^files?$",
                r"(?i)^storage$",
                r"(?i)^assets$",
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bgrpc\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "gRPC communication is indicated",
                        "gRPC integration is referenced for protobuf communication",
                    ),
                ),
                (
                    r"(?i)\bprotobuf\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Binary serialization is indicated by Protocol Buffers",
                        "Protobuf serialization is referenced for binary communication",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^proto$",
                r"(?i)^grpc$",
                r"(?i)^protobuf$",
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bgraphql\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL API is indicated",
                        "GraphQL schema-driven API is referenced",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^graphql$",
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bsmtp\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Email sending protocol is indicated",
                        "SMTP outbound email is referenced",
                    ),
                ),
                (
                    r"(?i)\bemail\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Email communication is indicated",
                        "Email integration is referenced for mail communication",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^email$",
                r"(?i)^mail$",
                r"(?i)^notifications?$",
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bcache\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache integration is indicated",
                        "Cache integration is referenced for temporary data storage",
                    ),
                ),
                (
                    r"(?i)\bredis\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "In-memory data store is indicated",
                        "Redis cache data store is referenced",
                    ),
                ),
                (
                    r"(?i)\bmemcached\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Distributed memory cache is indicated by Memcached",
                        "Memcached distributed caching is referenced",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^cache$",
                r"(?i)^caching$",
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bserver.sent.events?\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Server push streaming is indicated via Server-Sent Events",
                        "Server-sent events streaming is referenced",
                    ),
                ),
                (
                    r"(?i)\bsse\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Server-sent events are streamed to clients",
                        "SSE server push streaming is referenced",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^streaming$",
                r"(?i)^sse$",
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bcron\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduled task execution is indicated by Cron keyword",
                        "Cron scheduling is referenced for time-based jobs",
                    ),
                ),
                (
                    r"(?i)\bscheduler?\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduled tasks are indicated",
                        "Scheduler integration is referenced for automated tasks",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^scheduler$",
                r"(?i)^jobs$",
                r"(?i)^cron$",
                r"(?i)^tasks$",
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"(?i)\bftp\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File transfer is indicated via FTP",
                        "FTP file transfer protocol is referenced",
                    ),
                ),
                (
                    r"(?i)\bsftp\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Secure file transfer is indicated",
                        "SFTP secure file transfer is referenced",
                    ),
                ),
            ],
            PatternKey.DIRECTORY_PATTERNS: [
                r"(?i)^ftp$",
                r"(?i)^sftp$",
            ],
        },
    },
)
