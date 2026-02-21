"""Go base integration patterns."""

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
                    r'"net/http"',
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go net/http package import providing HTTP client and server primitives",
                        "This code uses Go net/http to interact with HTTP services",
                    ),
                ),
                (
                    r"http\.HandleFunc",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go http.HandleFunc registering an HTTP request handler function",
                        "This code uses Go net/http to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"http\.Handle\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go http.Handle registering an HTTP handler for a URL pattern",
                        "This code uses Go net/http to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"\.GET\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go router .GET method registering an HTTP GET route handler",
                        "This code uses Go net/http to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"\.POST\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go router .POST method registering an HTTP POST route handler",
                        "This code uses Go net/http to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"\.PUT\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go router .PUT method registering an HTTP PUT route handler",
                        "This code uses Go net/http to handle incoming HTTP PUT requests",
                    ),
                ),
                (
                    r"\.DELETE\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go router .DELETE method registering an HTTP DELETE route handler",
                        "This code uses Go net/http to handle incoming HTTP DELETE requests",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/hooklift/gowsdl"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go gowsdl package import generating a SOAP client from a WSDL",
                        "This code uses Go gowsdl to call an outbound SOAP service",
                    ),
                ),
                (
                    r'"encoding/xml"',
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go encoding/xml package import for XML serialization and deserialization",
                        "This code uses Go encoding/xml to interact with XML-based services",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/Shopify/sarama"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go sarama package import for Apache Kafka producer and consumer integration",
                        "This code uses Go sarama to interact with a Kafka message broker",
                    ),
                ),
                (
                    r'"github\.com/streadway/amqp"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go amqp package import for AMQP message broker integration",
                        "This code uses Go amqp to interact with an AMQP message broker",
                    ),
                ),
                (
                    r'"github\.com/nats-io/nats\.go"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go nats.go package import for NATS messaging system integration",
                        "This code uses Go NATS to interact with a NATS message broker",
                    ),
                ),
                (
                    r"sarama\.NewConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go sarama.NewConsumer function creating a Kafka consumer",
                        "This code uses Go sarama to receive messages from a Kafka topic",
                    ),
                ),
                (
                    r"sarama\.NewProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go sarama.NewProducer function creating a Kafka producer",
                        "This code uses Go sarama to produce messages to a Kafka topic",
                    ),
                ),
                (
                    r"amqp\.Dial",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go amqp.Dial function opening a connection to an AMQP broker",
                        "This code uses Go amqp to interact with an AMQP message broker",
                    ),
                ),
                (
                    r'"cloud\.google\.com/go/pubsub"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go pubsub package import for Google Cloud Pub/Sub integration",
                        "This code uses Go Cloud Pub/Sub to interact with a Google Cloud message topic",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/gorilla/websocket"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go gorilla/websocket package import for WebSocket client and server integration",
                        "This code uses Go Gorilla WebSocket to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"websocket\.Upgrader",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go websocket.Upgrader struct upgrading an HTTP connection to a WebSocket",
                        "This code uses Go Gorilla WebSocket to accept inbound WebSocket connections",
                    ),
                ),
                (
                    r"net\.Listen\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Go net.Listen function opening a TCP or Unix socket listener",
                        "This code uses Go net to listen for inbound network connections",
                    ),
                ),
                (
                    r"net\.Dial\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Go net.Dial function opening an outbound network connection",
                        "This code uses Go net to connect to an outbound network endpoint",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r'"gorm\.io/gorm"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go GORM package import for ORM-based database access",
                        "This code uses Go GORM to query a database",
                    ),
                ),
                (
                    r'"github\.com/jmoiron/sqlx"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go sqlx package import providing extensions to the standard database/sql library",
                        "This code uses Go sqlx to query a database",
                    ),
                ),
                (
                    r'"github\.com/jackc/pgx"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go pgx package import for PostgreSQL database driver access",
                        "This code uses Go pgx to connect to a PostgreSQL database",
                    ),
                ),
                (
                    r'"database/sql"',
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Go database/sql package import providing a generic SQL database interface",
                        "This code uses Go database/sql to interact with a relational database",
                    ),
                ),
                (
                    r"gorm\.Model",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go GORM model struct for ORM database mapping",
                        "This code uses Go GORM to define a database model for persistence",
                    ),
                ),
                (
                    r"db\.Create\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go GORM db.Create method inserting a new record into a database",
                        "This code uses Go GORM to write a new record to a database",
                    ),
                ),
                (
                    r"db\.Find\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go GORM db.Find method querying records from a database",
                        "This code uses Go GORM to query records from a database",
                    ),
                ),
                (
                    r"db\.Where\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go GORM db.Where method adding filter conditions to a database query",
                        "This code uses Go GORM to query filtered records from a database",
                    ),
                ),
                (
                    r"sql\.Open\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go sql.Open function opening a connection to a SQL database driver",
                        "This code uses Go database/sql to connect to an outbound SQL database",
                    ),
                ),
                (
                    r'"github\.com/gocql/gocql"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go gocql package import for Apache Cassandra database access",
                        "This code uses Go gocql to connect to a Cassandra database",
                    ),
                ),
                (
                    r'"cloud\.google\.com/go/firestore"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go Firestore package import for Google Cloud Firestore database access",
                        "This code uses Go Cloud Firestore to write to a Firestore database",
                    ),
                ),
                (
                    r'"github\.com/neo4j/neo4j-go-driver"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go neo4j-go-driver package import for Neo4j graph database access",
                        "This code uses Go Neo4j driver to connect to a Neo4j graph database",
                    ),
                ),
                (
                    r"neo4j\.NewDriverWithContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go neo4j.NewDriverWithContext function creating a Neo4j driver instance",
                        "This code uses Go Neo4j driver to connect to a Neo4j graph database",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"os\.Open\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go os.Open function opening a file for reading",
                        "This code uses Go os to read from a file",
                    ),
                ),
                (
                    r"os\.Create\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go os.Create function creating or truncating a file for writing",
                        "This code uses Go os to write to a file",
                    ),
                ),
                (
                    r"io\.Copy\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go io.Copy function copying data between an io.Reader and io.Writer",
                        "This code uses Go io to interact with a data stream",
                    ),
                ),
                (
                    r"s3manager",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go s3manager utility managing multipart uploads and downloads to AWS S3",
                        "This code uses Go AWS SDK to write to an S3 storage bucket",
                    ),
                ),
                (
                    r'"cloud\.google\.com/go/storage"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go Cloud Storage package import for Google Cloud Storage bucket access",
                        "This code uses Go Cloud Storage to write to a Google Cloud Storage bucket",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r'"google\.golang\.org/grpc"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go grpc package import providing core gRPC client and server primitives",
                        "This code uses Go gRPC to interact with a gRPC service",
                    ),
                ),
                (
                    r"pb\.Register.*Server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go pb.Register*Server function registering a gRPC service implementation",
                        "This code uses Go gRPC to expose an inbound gRPC service",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/99designs/gqlgen"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go gqlgen package import for code-generated GraphQL server integration",
                        "This code uses Go gqlgen to interact with a GraphQL API",
                    ),
                ),
                (
                    r'"github\.com/graphql-go/graphql"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go graphql-go package import for building a GraphQL schema and server",
                        "This code uses Go graphql-go to interact with a GraphQL API",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r'"net/smtp"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go net/smtp package import for sending email via an SMTP server",
                        "This code uses Go net/smtp to send outbound email messages",
                    ),
                ),
                (
                    r'"gopkg\.in/gomail"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go gomail package import for composing and sending email messages",
                        "This code uses Go gomail to send outbound email messages",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/go-redis/redis"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go go-redis package import for Redis cache client integration",
                        "This code uses Go go-redis to write to a Redis cache",
                    ),
                ),
                (
                    r'"github\.com/bradfitz/gomemcache"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go gomemcache package import for Memcached cache client integration",
                        "This code uses Go gomemcache to write to a Memcached cache",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Go text/event-stream content type enabling Server-Sent Events responses",
                        "This code uses Go net/http to expose an inbound Server-Sent Events stream",
                    ),
                ),
                (
                    r"http\.Flusher",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Go http.Flusher interface flushing buffered data to a streaming HTTP client",
                        "This code uses Go net/http to expose an inbound streaming HTTP response",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/pkg/sftp"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go pkg/sftp package import for SFTP client file transfer",
                        "This code uses Go pkg/sftp to connect to an outbound SFTP server",
                    ),
                ),
                (
                    r'"github\.com/jlaffaye/ftp"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Go jlaffaye/ftp package import for FTP client file transfer",
                        "This code uses Go jlaffaye/ftp to connect to an outbound FTP server",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/robfig/cron"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go robfig/cron package import for cron-based job scheduling",
                        "This code uses Go cron to integrate with a scheduled job runner",
                    ),
                ),
                (
                    r"time\.NewTicker",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Go time.NewTicker function creating a channel-based periodic timer",
                        "This code uses Go time to integrate with a periodic tick-based scheduler",
                    ),
                ),
            ],
        },
    },
)
