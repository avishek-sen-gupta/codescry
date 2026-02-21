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
                        "Kotlin Ktor client import for outbound HTTP requests",
                        "This code uses Kotlin Ktor to send outbound HTTP requests",
                    ),
                ),
                (
                    r"import okhttp3\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin OkHttp library import for HTTP client",
                        "This code uses Kotlin OkHttp to send outbound HTTP requests",
                    ),
                ),
                (
                    r"OkHttpClient\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin OkHttpClient instantiation for HTTP client",
                        "This code uses Kotlin OkHttp to send outbound HTTP requests",
                    ),
                ),
                (
                    r"import retrofit2\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Retrofit2 library import for REST client",
                        "This code uses Kotlin Retrofit2 to call outbound REST APIs",
                    ),
                ),
                (
                    r"HttpURLConnection",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin HttpURLConnection usage for HTTP client",
                        "This code uses Kotlin to send outbound HTTP requests",
                    ),
                ),
                (
                    r"import java\.net\.http\.HttpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Java HttpClient import for HTTP client",
                        "This code uses Kotlin Java HTTP client to send outbound HTTP requests",
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
                        "Kotlin JAX-WS @WebService annotation for SOAP service",
                        "This code uses Kotlin JAX-WS to expose an inbound SOAP web service",
                    ),
                ),
                (
                    r"@WebMethod",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin JAX-WS @WebMethod annotation for SOAP endpoint",
                        "This code uses Kotlin JAX-WS to handle inbound SOAP method calls",
                    ),
                ),
                (
                    r"SOAPMessage",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin SAAJ SOAPMessage for SOAP messaging",
                        "This code uses Kotlin SAAJ to interact with SOAP messages",
                    ),
                ),
                (
                    r"JAXBContext",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin JAXB JAXBContext for XML serialization",
                        "This code uses Kotlin JAXB to interact with XML data binding",
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
                        "Kotlin Apache Kafka import for messaging",
                        "This code uses Kotlin Apache Kafka to interact with a message broker",
                    ),
                ),
                (
                    r"KafkaProducer\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin KafkaProducer instantiation for message publishing",
                        "This code uses Kotlin Kafka to produce outgoing messages to a topic",
                    ),
                ),
                (
                    r"KafkaConsumer\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin KafkaConsumer instantiation for message consumption",
                        "This code uses Kotlin Kafka to receive inbound messages from a topic",
                    ),
                ),
                (
                    r"import com\.rabbitmq",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin RabbitMQ import for messaging",
                        "This code uses Kotlin RabbitMQ to interact with a message queue",
                    ),
                ),
                (
                    r"SqsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin AWS SqsClient for SQS queue operations",
                        "This code uses Kotlin AWS SDK to send messages to an SQS queue",
                    ),
                ),
                (
                    r"SnsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin AWS SnsClient for SNS notification publishing",
                        "This code uses Kotlin AWS SDK to produce outgoing SNS notifications",
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
                        "Kotlin ServerSocket instantiation for TCP server",
                        "This code uses Kotlin to accept inbound TCP socket connections",
                    ),
                ),
                (
                    r"DatagramSocket\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin DatagramSocket instantiation for UDP communication",
                        "This code uses Kotlin to interact with UDP datagrams",
                    ),
                ),
                (
                    r"import java\.nio\.channels",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin Java NIO channels import for non-blocking I/O",
                        "This code uses Kotlin Java NIO to interact with network channels",
                    ),
                ),
                (
                    r"@ServerEndpoint",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin JAX-WS @ServerEndpoint annotation for WebSocket server",
                        "This code uses Kotlin WebSocket to handle inbound WebSocket connections",
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
                        "Kotlin Exposed ORM import for database access",
                        "This code uses Kotlin Exposed to query a relational database",
                    ),
                ),
                (
                    r"Database\.connect\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Exposed Database.connect call for database connectivity",
                        "This code uses Kotlin Exposed to connect to a database",
                    ),
                ),
                (
                    r"transaction\s*\{",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Exposed transaction block for database operations",
                        "This code uses Kotlin Exposed to write to a database within a transaction",
                    ),
                ),
                (
                    r"@Entity",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin JPA @Entity annotation for database entity mapping",
                        "This code uses Kotlin JPA to query a relational database entity",
                    ),
                ),
                (
                    r"@Table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin JPA @Table annotation for database table mapping",
                        "This code uses Kotlin JPA to query a database table",
                    ),
                ),
                (
                    r"@Transactional",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Spring @Transactional annotation for transaction management",
                        "This code uses Kotlin Spring to write to a database transactionally",
                    ),
                ),
                (
                    r"EntityManager",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin JPA EntityManager for database entity operations",
                        "This code uses Kotlin JPA to query and write to a database",
                    ),
                ),
                (
                    r"PreparedStatement",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin JDBC PreparedStatement for parameterised SQL execution",
                        "This code uses Kotlin JDBC to query a relational database",
                    ),
                ),
                (
                    r"import org\.neo4j\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Neo4j driver import for graph database access",
                        "This code uses Kotlin Neo4j to query a graph database",
                    ),
                ),
                (
                    r"GraphDatabase\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Neo4j GraphDatabase.driver for graph database connectivity",
                        "This code uses Kotlin Neo4j to connect to a graph database",
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
                        "Kotlin File.readText call for file reading",
                        "This code uses Kotlin to receive data from a local file",
                    ),
                ),
                (
                    r"File\(.*\)\.writeText\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin File.writeText call for file writing",
                        "This code uses Kotlin to write to a local file",
                    ),
                ),
                (
                    r"import java\.nio\.file\.Files",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin Java NIO Files import for file system access",
                        "This code uses Kotlin Java NIO to interact with the file system",
                    ),
                ),
                (
                    r"AmazonS3Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin AWS AmazonS3Client for S3 object storage",
                        "This code uses Kotlin AWS SDK to write to S3 object storage",
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
                        "Kotlin gRPC import for RPC communication",
                        "This code uses Kotlin gRPC to interact with a remote procedure call service",
                    ),
                ),
                (
                    r"@GrpcService",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kotlin gRPC @GrpcService annotation for server-side RPC",
                        "This code uses Kotlin gRPC to handle inbound RPC calls",
                    ),
                ),
                (
                    r"StreamObserver",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin gRPC StreamObserver for streaming RPC",
                        "This code uses Kotlin gRPC to interact with streaming RPC calls",
                    ),
                ),
                (
                    r"ManagedChannel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin gRPC ManagedChannel for client-side RPC channel",
                        "This code uses Kotlin gRPC to call outbound RPC services",
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
                        "Kotlin GraphQL library import for GraphQL integration",
                        "This code uses Kotlin GraphQL to interact with a GraphQL API",
                    ),
                ),
                (
                    r"GraphQLSchema",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin GraphQL GraphQLSchema for schema definition",
                        "This code uses Kotlin GraphQL to interact with a GraphQL schema",
                    ),
                ),
                (
                    r"import com\.expediagroup\.graphql",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin GraphQL Kotlin library import for GraphQL server/client",
                        "This code uses Kotlin GraphQL Kotlin to interact with a GraphQL API",
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
                        "Kotlin JavaMail import for email sending",
                        "This code uses Kotlin JavaMail to send outbound email messages",
                    ),
                ),
                (
                    r"import jakarta\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Jakarta Mail import for email sending",
                        "This code uses Kotlin Jakarta Mail to send outbound email messages",
                    ),
                ),
                (
                    r"MimeMessage\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin MimeMessage instantiation for email composition",
                        "This code uses Kotlin JavaMail to send outbound MIME email messages",
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
                        "Kotlin Jedis import for Redis cache access",
                        "This code uses Kotlin Jedis to write to a Redis cache",
                    ),
                ),
                (
                    r"import io\.lettuce",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin Lettuce import for Redis cache access",
                        "This code uses Kotlin Lettuce to write to a Redis cache",
                    ),
                ),
                (
                    r"import javax\.cache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin JCache import for cache abstraction",
                        "This code uses Kotlin JCache to interact with a caching layer",
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
                        "Kotlin Spring SseEmitter for server-sent events",
                        "This code uses Kotlin Spring to expose inbound server-sent event streams",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin SSE content type for event streaming",
                        "This code uses Kotlin to interact with server-sent event streams",
                    ),
                ),
                (
                    r"Flux<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin Reactor Flux for reactive streaming",
                        "This code uses Kotlin Reactor to interact with reactive data streams",
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
                        "Kotlin Apache Commons FTPClient for FTP file transfer",
                        "This code uses Kotlin Apache Commons to connect to an FTP server",
                    ),
                ),
                (
                    r"JSch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin JSch instantiation for SSH/SFTP connectivity",
                        "This code uses Kotlin JSch to connect to an SFTP server",
                    ),
                ),
                (
                    r"ChannelSftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kotlin JSch ChannelSftp for SFTP file operations",
                        "This code uses Kotlin JSch to write to an SFTP server",
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
                        "Kotlin Quartz scheduler import for job scheduling",
                        "This code uses Kotlin Quartz to interact with scheduled jobs",
                    ),
                ),
                (
                    r"@Scheduled",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin Spring @Scheduled annotation for task scheduling",
                        "This code uses Kotlin Spring to interact with scheduled tasks",
                    ),
                ),
                (
                    r"ScheduledExecutorService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin ScheduledExecutorService for thread-based scheduling",
                        "This code uses Kotlin to interact with scheduled executor tasks",
                    ),
                ),
                (
                    r"import kotlinx\.coroutines\.delay",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kotlin coroutines delay import for coroutine-based scheduling",
                        "This code uses Kotlin coroutines to interact with delayed execution",
                    ),
                ),
            ],
        },
    },
)
