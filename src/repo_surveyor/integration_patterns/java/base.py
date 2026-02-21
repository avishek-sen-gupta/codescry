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
                        "Java HttpServletRequest for handling incoming HTTP requests",
                        "This code uses Java Servlet API to process inbound HTTP requests",
                    ),
                ),
                (
                    r"HttpServletResponse",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java HttpServletResponse for sending HTTP responses",
                        "This code uses Java Servlet API to send HTTP responses",
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
                        "Java @WebService annotation defining a SOAP web service endpoint",
                        "This code uses Java JAX-WS to expose an inbound SOAP web service",
                    ),
                ),
                (
                    r"@WebMethod",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @WebMethod annotation exposing a method as a SOAP operation",
                        "This code uses Java JAX-WS to expose an inbound SOAP web service operation",
                    ),
                ),
                (
                    r"@WebParam",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @WebParam annotation mapping a SOAP operation parameter",
                        "This code uses Java JAX-WS to accept inbound SOAP web service parameters",
                    ),
                ),
                (
                    r"@SOAPBinding",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @SOAPBinding annotation configuring SOAP message binding style",
                        "This code uses Java JAX-WS to expose an inbound SOAP endpoint binding",
                    ),
                ),
                (
                    r"SOAPMessage",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java SOAPMessage class for constructing or parsing SOAP messages",
                        "This code uses Java SAAJ to interact with a SOAP web service",
                    ),
                ),
                (
                    r"SOAPEnvelope",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java SOAPEnvelope class for accessing the SOAP message envelope",
                        "This code uses Java SAAJ to interact with a SOAP endpoint",
                    ),
                ),
                (
                    r"JAXBContext",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java JAXBContext for XML marshalling and unmarshalling",
                        "This code uses Java JAXB to interact with XML-based SOAP messages",
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
                        "Java JMS MessageListener interface for receiving messages",
                        "This code uses Java JMS to receive incoming messages from a message queue",
                    ),
                ),
                (
                    r"MessageProducer",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Java JMS MessageProducer for sending messages to a destination",
                        "This code uses Java JMS to send outgoing messages to a message queue",
                    ),
                ),
                (
                    r"MessageConsumer",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Java JMS MessageConsumer for receiving messages from a destination",
                        "This code uses Java JMS to consume incoming messages from a message queue",
                    ),
                ),
                (
                    r"SqsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java AWS SqsClient for interacting with Amazon SQS queues",
                        "This code uses Java AWS SDK to send outgoing messages to an SQS queue",
                    ),
                ),
                (
                    r"SnsClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java AWS SnsClient for publishing messages to Amazon SNS topics",
                        "This code uses Java AWS SDK to send outgoing messages to an SNS topic",
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
                        "Java @ServerEndpoint annotation defining a WebSocket server endpoint",
                        "This code uses Java WebSocket API to expose an inbound WebSocket connection",
                    ),
                ),
                (
                    r"@OnOpen",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @OnOpen annotation handling WebSocket connection open events",
                        "This code uses Java WebSocket API to accept an inbound WebSocket connection",
                    ),
                ),
                (
                    r"@OnClose",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @OnClose annotation handling WebSocket connection close events",
                        "This code uses Java WebSocket API to handle inbound WebSocket connection closure",
                    ),
                ),
                (
                    r"@OnMessage",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @OnMessage annotation receiving WebSocket messages",
                        "This code uses Java WebSocket API to receive inbound WebSocket messages",
                    ),
                ),
                (
                    r"@OnError",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @OnError annotation handling WebSocket error events",
                        "This code uses Java WebSocket API to handle inbound WebSocket error events",
                    ),
                ),
                (
                    r"ServerSocket\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java ServerSocket class for accepting incoming TCP connections",
                        "This code uses Java networking to accept inbound TCP socket connections",
                    ),
                ),
                (
                    r"DatagramSocket",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java DatagramSocket class for sending and receiving UDP packets",
                        "This code uses Java networking to interact with a network socket via UDP",
                    ),
                ),
                (
                    r"SocketChannel",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java NIO SocketChannel for non-blocking TCP socket I/O",
                        "This code uses Java NIO to interact with a network socket",
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
                        "Java JPA @Entity annotation mapping a class to a database table",
                        "This code uses Java JPA to define a database entity for persistence",
                    ),
                ),
                (
                    r"@Table",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java JPA @Table annotation specifying the database table name",
                        "This code uses Java JPA to map a class to a specific database table",
                    ),
                ),
                (
                    r"@Column",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Java JPA @Column annotation mapping a field to a database column",
                        "This code uses Java JPA to map a field to a specific database column",
                    ),
                ),
                (
                    r"@JoinColumn",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Java JPA @JoinColumn annotation defining a foreign key column",
                        "This code uses Java JPA to write a foreign key relationship to a database table",
                    ),
                ),
                (
                    r"@OneToMany",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Java JPA @OneToMany annotation mapping a one-to-many relationship",
                        "This code uses Java JPA to write a one-to-many relationship to a database",
                    ),
                ),
                (
                    r"@ManyToOne",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Java JPA @ManyToOne annotation mapping a many-to-one relationship",
                        "This code uses Java JPA to write a many-to-one relationship to a database",
                    ),
                ),
                (
                    r"@ManyToMany",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Java JPA @ManyToMany annotation mapping a many-to-many relationship",
                        "This code uses Java JPA to write a many-to-many relationship to a database",
                    ),
                ),
                (
                    r"@Transactional",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java @Transactional annotation managing database transaction boundaries",
                        "This code uses Java JPA to execute database queries within a transaction",
                    ),
                ),
                (
                    r"@PersistenceContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java @PersistenceContext annotation injecting a JPA EntityManager",
                        "This code uses Java JPA to query a database via an injected EntityManager",
                    ),
                ),
                (
                    r"EntityManager",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java JPA EntityManager for database operations",
                        "This code uses Java JPA EntityManager to execute database queries",
                    ),
                ),
                (
                    r"PreparedStatement",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java JDBC PreparedStatement for executing SQL queries",
                        "This code uses Java JDBC to execute parameterized database queries",
                    ),
                ),
                (
                    r"ResultSet\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Java JDBC ResultSet for reading SQL query results",
                        "This code uses Java JDBC to query and read results from a database",
                    ),
                ),
                (
                    r"DynamoDbClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java AWS DynamoDbClient for accessing Amazon DynamoDB",
                        "This code uses Java AWS SDK to query an Amazon DynamoDB database",
                    ),
                ),
                (
                    r"import org\.neo4j\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java Neo4j driver import for connecting to a Neo4j graph database",
                        "This code uses Java Neo4j driver to query a Neo4j graph database",
                    ),
                ),
                (
                    r"GraphDatabase\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java Neo4j GraphDatabase.driver call for opening a database connection",
                        "This code uses Java Neo4j driver to connect to a graph database",
                    ),
                ),
                (
                    r"DriverManager\.getConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java JDBC DriverManager.getConnection call for opening a database connection",
                        "This code uses Java JDBC to connect to a relational database",
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
                        "Java FileInputStream for reading bytes from a file",
                        "This code uses Java I/O to read from a file",
                    ),
                ),
                (
                    r"FileOutputStream",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java FileOutputStream for writing bytes to a file",
                        "This code uses Java I/O to write to a file",
                    ),
                ),
                (
                    r"BufferedReader",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java BufferedReader for reading text from a character input stream",
                        "This code uses Java I/O to interact with a file or input stream",
                    ),
                ),
                (
                    r"Files\.read",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java NIO Files.read method for reading file contents",
                        "This code uses Java NIO to read from a file",
                    ),
                ),
                (
                    r"Files\.write",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java NIO Files.write method for writing content to a file",
                        "This code uses Java NIO to write to a file",
                    ),
                ),
                (
                    r"Files\.copy",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java NIO Files.copy method for copying file content",
                        "This code uses Java NIO to interact with a filesystem",
                    ),
                ),
                (
                    r"AmazonS3Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java AWS AmazonS3Client for accessing Amazon S3 object storage",
                        "This code uses Java AWS SDK to write to Amazon S3 cloud storage",
                    ),
                ),
                (
                    r"import com\.azure\.storage\.blob",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java Azure Blob Storage import for accessing Azure cloud storage",
                        "This code uses Java Azure SDK to write to Azure Blob cloud storage",
                    ),
                ),
                (
                    r"import com\.google\.cloud\.storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java Google Cloud Storage import for accessing GCS buckets",
                        "This code uses Java Google Cloud SDK to write to Google Cloud Storage",
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
                        "Java gRPC import for building gRPC clients or servers",
                        "This code uses Java gRPC to interact with a gRPC service",
                    ),
                ),
                (
                    r"@GrpcService",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Java @GrpcService annotation defining a gRPC server implementation",
                        "This code uses Java gRPC to expose an inbound gRPC service",
                    ),
                ),
                (
                    r"StreamObserver",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java gRPC StreamObserver for handling streaming RPC calls",
                        "This code uses Java gRPC to interact with a gRPC streaming endpoint",
                    ),
                ),
                (
                    r"ManagedChannel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java gRPC ManagedChannel for connecting to a remote gRPC server",
                        "This code uses Java gRPC to connect to an outbound gRPC service",
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
                        "Java GraphQL import for building or querying a GraphQL API",
                        "This code uses Java GraphQL to interact with a GraphQL schema",
                    ),
                ),
                (
                    r"GraphQLSchema",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java GraphQLSchema class for defining a GraphQL schema",
                        "This code uses Java GraphQL to interact with a GraphQL schema",
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
                        "Java javax.mail API for sending email messages",
                        "This code uses Java Mail API to send outgoing email messages",
                    ),
                ),
                (
                    r"jakarta\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java jakarta.mail API for sending email messages",
                        "This code uses Java Jakarta Mail to send outgoing email messages",
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
                        "Java Jedis import for connecting to a Redis cache",
                        "This code uses Java Jedis to connect to a Redis cache store",
                    ),
                ),
                (
                    r"import io\.lettuce",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java Lettuce import for connecting to a Redis cache",
                        "This code uses Java Lettuce to connect to a Redis cache store",
                    ),
                ),
                (
                    r"import javax\.cache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java JCache (JSR-107) import for interacting with a cache store",
                        "This code uses Java JCache API to interact with a cache store",
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
                        "Java SseEmitter class for pushing server-sent events to clients",
                        "This code uses Java SSE to expose an inbound server-sent event stream",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java text/event-stream content type for SSE responses",
                        "This code uses Java HTTP to interact with a server-sent event stream",
                    ),
                ),
                (
                    r"Flux<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java Reactor Flux type for reactive streaming data sequences",
                        "This code uses Java Reactor to interact with a server-sent event stream",
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
                        "Java Apache Commons FTPClient for connecting to an FTP server",
                        "This code uses Java FTP client to connect to an FTP server",
                    ),
                ),
                (
                    r"JSch",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java JSch library for establishing SSH/SFTP connections",
                        "This code uses Java JSch to connect to an SFTP connection",
                    ),
                ),
                (
                    r"ChannelSftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java JSch ChannelSftp for transferring files over SFTP",
                        "This code uses Java JSch to connect to an SFTP connection",
                    ),
                ),
                (
                    r"SFTPClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Java SFTPClient for connecting to and transferring files via SFTP",
                        "This code uses Java SFTP client to connect to an SFTP connection",
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
                        "Java Quartz Scheduler import for defining scheduled tasks",
                        "This code uses Java Quartz to interact with a scheduled task",
                    ),
                ),
                (
                    r"@Schedule",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java @Schedule annotation for defining a scheduled task",
                        "This code uses Java EJB to work with a scheduled task",
                    ),
                ),
                (
                    r"ScheduledExecutorService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Java ScheduledExecutorService for executing tasks on a schedule",
                        "This code uses Java concurrency utilities to work with a scheduled task",
                    ),
                ),
            ],
        },
    },
)
