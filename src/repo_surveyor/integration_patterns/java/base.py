"""Java base integration patterns."""

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
                    r"HttpServletRequest",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP requests are handled by servlet request",
                        "HTTP requests are processed by servlet",
                    ),
                ),
                (
                    r"HttpServletResponse",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP responses are sent by servlet response",
                        "HTTP responses are sent by servlet",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"@WebService",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP web service endpoint is annotated",
                        "SOAP service is exposed for inbound requests",
                    ),
                ),
                (
                    r"@WebMethod",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP operation is exposed by method annotation",
                        "SOAP operation is exposed for inbound requests",
                    ),
                ),
                (
                    r"@WebParam",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP parameter is mapped by annotation",
                        "SOAP parameters are accepted by inbound service",
                    ),
                ),
                (
                    r"@SOAPBinding",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP message binding is configured by annotation",
                        "SOAP endpoint is bound for inbound requests",
                    ),
                ),
                (
                    r"SOAPMessage",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP messages are constructed or parsed",
                        "SOAP service is accessed via SAAJ",
                    ),
                ),
                (
                    r"SOAPEnvelope",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP message envelope is accessed",
                        "SOAP endpoint is accessed via SAAJ",
                    ),
                ),
                (
                    r"JAXBContext",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML marshalling is handled by JAXB context",
                        "XML messages are processed via JAXB",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"MessageListener",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Messages are received via MessageListener interface",
                        "Messages are received from inbound queue",
                    ),
                ),
                (
                    r"MessageProducer",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Messages are sent to JMS destination",
                        "Messages are sent to outbound queue",
                    ),
                ),
                (
                    r"MessageConsumer",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Messages are received from JMS destination",
                        "Messages are consumed from inbound queue",
                    ),
                ),
                (
                    r"SqsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQS queues are accessed by client",
                        "SQS message is sent outbound",
                    ),
                ),
                (
                    r"SnsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SNS messages are published to topics",
                        "SNS message is sent outbound",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"@ServerEndpoint",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket server endpoint is annotated",
                        "WebSocket connection is exposed for inbound requests",
                    ),
                ),
                (
                    r"@OnOpen",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket open events are handled by annotation",
                        "WebSocket connection is accepted for inbound requests",
                    ),
                ),
                (
                    r"@OnClose",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket close events are handled by annotation",
                        "WebSocket connection closure is handled",
                    ),
                ),
                (
                    r"@OnMessage",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket messages are received by annotation",
                        "WebSocket messages are received from inbound connection",
                    ),
                ),
                (
                    r"@OnError",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket error events are handled by annotation",
                        "WebSocket error events are handled",
                    ),
                ),
                (
                    r"ServerSocket\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TCP connections are accepted by ServerSocket",
                        "TCP connections are accepted for inbound sockets",
                    ),
                ),
                (
                    r"DatagramSocket",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP packets are sent via datagram socket",
                        "Network socket is accessed via UDP",
                    ),
                ),
                (
                    r"SocketChannel",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP socket is opened for non-blocking IO",
                        "Network socket is accessed via NIO",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"@Entity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is mapped with class annotation",
                        "Database entity is defined for persistence",
                    ),
                ),
                (
                    r"@Table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table name is specified with annotation",
                        "Class is mapped to database table",
                    ),
                ),
                (
                    r"@Column",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is mapped with field annotation",
                        "Field is mapped to database column",
                    ),
                ),
                (
                    r"@JoinColumn",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Foreign key column is defined with annotation",
                        "Foreign key relationship is written to database",
                    ),
                ),
                (
                    r"@OneToMany",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "One-to-many relationship is mapped with annotation",
                        "One-to-many relationship is written to database",
                    ),
                ),
                (
                    r"@ManyToOne",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Many-to-one relationship is mapped with annotation",
                        "Many-to-one relationship is written to database",
                    ),
                ),
                (
                    r"@ManyToMany",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Many-to-many relationship is mapped with annotation",
                        "Many-to-many relationship is written to database",
                    ),
                ),
                (
                    r"@Transactional",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database transactions are managed by annotation",
                        "Database queries are executed within transaction",
                    ),
                ),
                (
                    r"@PersistenceContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JPA EntityManager is injected for persistence",
                        "Database is queried via injected EntityManager",
                    ),
                ),
                (
                    r"EntityManager",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are managed by EntityManager",
                        "Database queries are executed via EntityManager",
                    ),
                ),
                (
                    r"PreparedStatement",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL queries are executed by prepared statement",
                        "Database queries are executed with parameters",
                    ),
                ),
                (
                    r"ResultSet\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query results are read from ResultSet",
                        "Database results are queried and read",
                    ),
                ),
                (
                    r"DynamoDbClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "DynamoDB is accessed by client",
                        "DynamoDB database is queried via AWS SDK",
                    ),
                ),
                (
                    r"import org\.neo4j\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j graph database is connected via driver",
                        "Neo4j database is queried via driver",
                    ),
                ),
                (
                    r"GraphDatabase\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened to Neo4j",
                        "Graph database connection is opened via Neo4j",
                    ),
                ),
                (
                    r"DriverManager\.getConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened by driver manager",
                        "Database connection is opened via JDBC",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"FileInputStream",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "File bytes are read by input stream",
                        "File is read via I/O stream",
                    ),
                ),
                (
                    r"FileOutputStream",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File bytes are written by output stream",
                        "File is written via I/O stream",
                    ),
                ),
                (
                    r"BufferedReader",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Text input stream is read by buffer",
                        "File stream is accessed for I/O operations",
                    ),
                ),
                (
                    r"Files\.read",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "File contents are read from path",
                        "File is read via NIO",
                    ),
                ),
                (
                    r"Files\.write",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Content is written to file path",
                        "File is written via NIO",
                    ),
                ),
                (
                    r"Files\.copy",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File content is copied between paths",
                        "Filesystem is accessed via NIO",
                    ),
                ),
                (
                    r"AmazonS3Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 object storage is accessed by client",
                        "S3 storage is written via AWS SDK",
                    ),
                ),
                (
                    r"import com\.azure\.storage\.blob",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Azure cloud storage is imported for access",
                        "Azure Blob storage is written",
                    ),
                ),
                (
                    r"import com\.google\.cloud\.storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "GCS buckets are imported for access",
                        "Cloud Storage is written via Google SDK",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.grpc",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "gRPC clients are built with import",
                        "gRPC service is accessed for communication",
                    ),
                ),
                (
                    r"@GrpcService",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC server implementation is annotated",
                        "gRPC service is exposed for inbound requests",
                    ),
                ),
                (
                    r"StreamObserver",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Streaming RPC calls are handled",
                        "gRPC streaming endpoint is accessed",
                    ),
                ),
                (
                    r"ManagedChannel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "gRPC server is connected via channel",
                        "gRPC service is connected for outbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"import graphql\.",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL API is imported for building",
                        "GraphQL schema is accessed via client",
                    ),
                ),
                (
                    r"GraphQLSchema",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL schema is defined by class",
                        "GraphQL schema is accessed via client",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"javax\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email messages are sent via mail API",
                        "Email messages are sent via Mail API",
                    ),
                ),
                (
                    r"jakarta\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email messages are sent via mail API",
                        "Email messages are sent via Jakarta Mail",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"import redis\.clients\.jedis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is connected via Jedis",
                        "Redis connection is opened via Jedis",
                    ),
                ),
                (
                    r"import io\.lettuce",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is connected via Lettuce",
                        "Redis connection is opened via Lettuce",
                    ),
                ),
                (
                    r"import javax\.cache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache store is imported via JCache",
                        "Cache store is accessed via JCache API",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"SseEmitter",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-sent events are pushed to clients",
                        "Event stream is exposed for inbound SSE",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SSE responses are streamed with content type",
                        "Server-sent event stream is accessed via HTTP",
                    ),
                ),
                (
                    r"Flux<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Data sequences are streamed via Flux",
                        "Event stream is processed via Reactor",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"FTPClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP server connection is established by client",
                        "FTP server connection is opened",
                    ),
                ),
                (
                    r"JSch",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SSH connection is established for SFTP",
                        "SFTP connection is opened via JSch",
                    ),
                ),
                (
                    r"ChannelSftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Files are transferred over SFTP channel",
                        "SFTP connection is opened via JSch",
                    ),
                ),
                (
                    r"SFTPClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Files are transferred via SFTP client",
                        "SFTP connection is opened via client",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.quartz",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduled tasks are defined with Quartz",
                        "Scheduled task is managed via Quartz",
                    ),
                ),
                (
                    r"@Schedule",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduled task is defined by annotation",
                        "Scheduled task is integrated with EJB",
                    ),
                ),
                (
                    r"ScheduledExecutorService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Tasks are executed on schedule",
                        "Scheduled task is managed via concurrency utilities",
                    ),
                ),
            ],
        },
    },
)
