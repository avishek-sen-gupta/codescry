"""Kotlin base integration patterns."""

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
                    r"import io\.ktor\.client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported for outbound requests",
                        "HTTP requests are sent outbound",
                    ),
                ),
                (
                    r"import okhttp3\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported via OkHttp",
                        "HTTP requests are sent outbound",
                    ),
                ),
                (
                    r"OkHttpClient\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is created for requests",
                        "HTTP requests are sent outbound",
                    ),
                ),
                (
                    r"import retrofit2\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "REST client is imported via Retrofit",
                        "REST APIs are called outbound",
                    ),
                ),
                (
                    r"HttpURLConnection",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP connection is opened for outbound request",
                        "HTTP requests are sent outbound",
                    ),
                ),
                (
                    r"import java\.net\.http\.HttpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported for requests",
                        "HTTP requests are sent outbound",
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
                        "SOAP service is annotated for web service",
                        "SOAP web service is exposed for inbound requests",
                    ),
                ),
                (
                    r"@WebMethod",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SOAP endpoint is annotated with web method",
                        "SOAP method calls are handled for inbound requests",
                    ),
                ),
                (
                    r"SOAPMessage",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP message is created for messaging",
                        "SOAP messages are processed",
                    ),
                ),
                (
                    r"JAXBContext",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML serialization is handled by JAXB context",
                        "XML data is bound via JAXB",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.apache\.kafka",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Messaging is handled via Kafka import",
                        "Message broker is accessed via Kafka",
                    ),
                ),
                (
                    r"KafkaProducer\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Message producer is created for Kafka",
                        "Messages are produced to Kafka topic",
                    ),
                ),
                (
                    r"KafkaConsumer\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Message consumer is created for Kafka",
                        "Messages are received from Kafka topic",
                    ),
                ),
                (
                    r"import com\.rabbitmq",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Messaging is imported via RabbitMQ",
                        "Message queue is accessed",
                    ),
                ),
                (
                    r"SqsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQS queue operations are performed via client",
                        "Messages are sent to SQS queue",
                    ),
                ),
                (
                    r"SnsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SNS notifications are published via client",
                        "SNS notifications are produced for outgoing messages",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"ServerSocket\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TCP server is created via socket",
                        "TCP socket connections are accepted inbound",
                    ),
                ),
                (
                    r"DatagramSocket\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP communication is established via DatagramSocket",
                        "UDP datagrams are processed",
                    ),
                ),
                (
                    r"import java\.nio\.channels",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Non-blocking I/O is imported via channels",
                        "Network channels are accessed via NIO",
                    ),
                ),
                (
                    r"@ServerEndpoint",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket server is annotated with endpoint",
                        "WebSocket connections are handled for inbound clients",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.jetbrains\.exposed",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is accessed via Exposed ORM",
                        "Database is queried via relational interface",
                    ),
                ),
                (
                    r"Database\.connect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is connected via Exposed",
                        "Database connection is opened",
                    ),
                ),
                (
                    r"transaction\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database operations are executed in transaction",
                        "Database is written within transaction",
                    ),
                ),
                (
                    r"@Entity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database entity is annotated for mapping",
                        "Database entity is queried via JPA",
                    ),
                ),
                (
                    r"@Table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is annotated for mapping",
                        "Database table is queried via JPA",
                    ),
                ),
                (
                    r"@Transactional",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Transaction management is annotated for handling",
                        "Database is written transactionally",
                    ),
                ),
                (
                    r"EntityManager",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database entities are managed by entity manager",
                        "Database is queried and written via JPA",
                    ),
                ),
                (
                    r"PreparedStatement",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL statement is prepared with parameters",
                        "Database is queried via JDBC interface",
                    ),
                ),
                (
                    r"import org\.neo4j\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is imported via driver",
                        "Graph database is queried",
                    ),
                ),
                (
                    r"GraphDatabase\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is connected via driver",
                        "Graph database connection is opened",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"File\(.*\)\.readText\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "File content is read from path",
                        "Data is read from local file",
                    ),
                ),
                (
                    r"File\(.*\)\.writeText\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "File content is written to filesystem",
                        "Data is written to local file",
                    ),
                ),
                (
                    r"import java\.nio\.file\.Files",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File system is accessed via NIO Files",
                        "File system is accessed via NIO",
                    ),
                ),
                (
                    r"AmazonS3Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 object storage is accessed via client",
                        "Objects are written to S3 storage",
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
                        "gRPC communication is imported",
                        "RPC service is accessed remotely",
                    ),
                ),
                (
                    r"@GrpcService",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC service is annotated for RPC handling",
                        "RPC calls are handled for inbound requests",
                    ),
                ),
                (
                    r"StreamObserver",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "RPC streaming is handled with StreamObserver",
                        "Streaming RPC calls are managed",
                    ),
                ),
                (
                    r"ManagedChannel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "gRPC channel is created for client RPC",
                        "RPC services are called for outbound requests",
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
                        "GraphQL library is imported for integration",
                        "GraphQL API is accessed",
                    ),
                ),
                (
                    r"GraphQLSchema",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL schema is defined for API structure",
                        "GraphQL schema is accessed",
                    ),
                ),
                (
                    r"import com\.expediagroup\.graphql",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL library is imported for server client",
                        "GraphQL API is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"import javax\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email sending is imported via JavaMail",
                        "Email messages are sent outbound",
                    ),
                ),
                (
                    r"import jakarta\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email sending is imported via Jakarta Mail",
                        "Email messages are sent outbound",
                    ),
                ),
                (
                    r"MimeMessage\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is created for composition",
                        "MIME email messages are sent outbound",
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
                        "Redis cache is accessed via Jedis",
                        "Redis cache is written to",
                    ),
                ),
                (
                    r"import io\.lettuce",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is accessed via Lettuce",
                        "Redis cache is written to",
                    ),
                ),
                (
                    r"import javax\.cache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache abstraction is imported via JCache",
                        "Cache is accessed via JCache interface",
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
                        "Server-sent events are emitted via SseEmitter",
                        "Server-sent event streams are exposed for inbound requests",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Event streaming is indicated by SSE",
                        "Server-sent event streams are handled",
                    ),
                ),
                (
                    r"Flux<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Reactive stream is handled by Flux",
                        "Reactive data streams are processed",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"FTPClient\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP file transfer is performed via client",
                        "FTP server connection is opened",
                    ),
                ),
                (
                    r"JSch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SSH connectivity is established via JSch",
                        "SFTP server connection is opened",
                    ),
                ),
                (
                    r"ChannelSftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP operations are handled by channel",
                        "SFTP server is written to",
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
                        "Job scheduler is imported via Quartz",
                        "Scheduled jobs are managed",
                    ),
                ),
                (
                    r"@Scheduled",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Task scheduling is annotated for execution",
                        "Scheduled tasks are executed using Spring framework",
                    ),
                ),
                (
                    r"ScheduledExecutorService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Thread scheduling is handled by executor service",
                        "Scheduled executor tasks are managed",
                    ),
                ),
                (
                    r"import kotlinx\.coroutines\.delay",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Coroutine scheduling is imported with delay",
                        "Delayed execution is managed with coroutines",
                    ),
                ),
            ],
        },
    },
)
