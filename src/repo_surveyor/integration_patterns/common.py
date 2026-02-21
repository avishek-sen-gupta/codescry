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
                        "Generic HTTP keyword indicating web communication",
                        "This code references HTTP suggesting web-based integration",
                    ),
                ),
                (
                    r"(?i)\brest(?:ful)?\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Generic REST or RESTful keyword indicating API-style integration",
                        "This code references REST suggesting HTTP API-based communication",
                    ),
                ),
                (
                    r"(?i)\bapi\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Generic API keyword indicating programmatic integration",
                        "This code references an API suggesting external service communication",
                    ),
                ),
                (
                    r"(?i)\bendpoint\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Generic endpoint keyword indicating a network-accessible interface",
                        "This code references an endpoint suggesting exposed or consumed service URLs",
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
                        "Generic SOAP keyword indicating XML web service communication",
                        "This code references SOAP suggesting XML-based web service integration",
                    ),
                ),
                (
                    r"(?i)\bwsdl\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WSDL descriptor indicating SOAP web service definition",
                        "This code references a WSDL descriptor for SOAP service interface definition",
                    ),
                ),
                (
                    r"(?i)\benvelope\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP envelope keyword indicating XML message wrapping",
                        "This code references an envelope suggesting SOAP XML message structure",
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
                        "Apache Kafka keyword indicating distributed event streaming",
                        "This code references Kafka suggesting message broker integration",
                    ),
                ),
                (
                    r"(?i)\brabbit(?:mq)?\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "RabbitMQ keyword indicating AMQP message broker usage",
                        "This code references RabbitMQ suggesting asynchronous message queue integration",
                    ),
                ),
                (
                    r"(?i)\bamqp\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP protocol keyword indicating message broker communication",
                        "This code references AMQP suggesting message queue protocol usage",
                    ),
                ),
                (
                    r"(?i)\bpulsar\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Apache Pulsar keyword indicating distributed messaging",
                        "This code references Pulsar suggesting distributed pub-sub messaging integration",
                    ),
                ),
                (
                    r"(?i)\bnats\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "NATS keyword indicating lightweight messaging system",
                        "This code references NATS suggesting lightweight pub-sub messaging integration",
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
                        "WebSocket keyword indicating bidirectional real-time communication",
                        "This code references WebSocket suggesting real-time bidirectional connection",
                    ),
                ),
                (
                    r"(?i)\btcp\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP keyword indicating transport-layer network communication",
                        "This code references TCP suggesting low-level network socket communication",
                    ),
                ),
                (
                    r"(?i)\budp\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP keyword indicating datagram-based network communication",
                        "This code references UDP suggesting connectionless datagram communication",
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
                        "Generic database keyword indicating data persistence",
                        "This code references a database suggesting data storage integration",
                    ),
                ),
                (
                    r"(?i)\bdatasource\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Datasource keyword indicating database connection configuration",
                        "This code references a datasource suggesting configured database connection",
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
                        "Generic file keyword indicating filesystem interaction",
                        "This code references file operations suggesting filesystem I/O integration",
                    ),
                ),
                (
                    r"(?i)\bupload\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Upload keyword indicating file transfer to a server",
                        "This code references upload suggesting inbound file transfer",
                    ),
                ),
                (
                    r"(?i)\bdownload\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Download keyword indicating file retrieval from a server",
                        "This code references download suggesting outbound file retrieval",
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
                        "gRPC keyword indicating remote procedure call framework",
                        "This code references gRPC suggesting protobuf-based remote procedure call integration",
                    ),
                ),
                (
                    r"(?i)\bprotobuf\b",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Protocol Buffers keyword indicating binary serialization",
                        "This code references protobuf suggesting gRPC or binary serialization integration",
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
                        "GraphQL keyword indicating query-based API",
                        "This code references GraphQL suggesting schema-driven API integration",
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
                        "SMTP keyword indicating email sending protocol",
                        "This code references SMTP suggesting outbound email integration",
                    ),
                ),
                (
                    r"(?i)\bemail\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Generic email keyword indicating mail communication",
                        "This code references email suggesting mail-based integration",
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
                        "Generic cache keyword indicating data caching",
                        "This code references caching suggesting temporary data storage integration",
                    ),
                ),
                (
                    r"(?i)\bredis\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Redis keyword indicating in-memory data store",
                        "This code references Redis suggesting in-memory cache or data store integration",
                    ),
                ),
                (
                    r"(?i)\bmemcached\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Memcached keyword indicating distributed memory cache",
                        "This code references Memcached suggesting distributed caching integration",
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
                        "Server-Sent Events keyword indicating server push streaming",
                        "This code references Server-Sent Events suggesting unidirectional server push streaming",
                    ),
                ),
                (
                    r"(?i)\bsse\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SSE keyword indicating server-sent event streaming",
                        "This code references SSE suggesting server push event streaming",
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
                        "Cron keyword indicating scheduled task execution",
                        "This code references cron suggesting time-based job scheduling",
                    ),
                ),
                (
                    r"(?i)\bscheduler?\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduler keyword indicating timed task management",
                        "This code references a scheduler suggesting automated task scheduling",
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
                        "FTP keyword indicating file transfer protocol",
                        "This code references FTP suggesting file transfer protocol integration",
                    ),
                ),
                (
                    r"(?i)\bsftp\b",
                    Confidence.LOW,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SFTP keyword indicating secure file transfer",
                        "This code references SFTP suggesting secure file transfer integration",
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
