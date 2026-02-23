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
                        "HTTP primitives are imported",
                        "HTTP service is accessed via client",
                    ),
                ),
                (
                    r"http\.HandleFunc",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP handler function is registered",
                        "HTTP request is handled inbound",
                    ),
                ),
                (
                    r"http\.Handle\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP handler is registered for URL pattern",
                        "HTTP request is handled inbound",
                    ),
                ),
                (
                    r"\.GET\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is registered",
                        "HTTP GET request is handled inbound",
                    ),
                ),
                (
                    r"\.POST\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is registered",
                        "HTTP POST request is handled inbound",
                    ),
                ),
                (
                    r"\.PUT\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT route is registered",
                        "HTTP PUT request is handled inbound",
                    ),
                ),
                (
                    r"\.DELETE\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE route is registered",
                        "HTTP DELETE request is handled inbound",
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
                        "SOAP client is generated from WSDL",
                        "SOAP service is called outbound",
                    ),
                ),
                (
                    r'"encoding/xml"',
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML serialization is imported",
                        "XML service is accessed via encoding",
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
                        "Kafka integration is imported via sarama package",
                        "Kafka broker is accessed via client",
                    ),
                ),
                (
                    r'"github\.com/streadway/amqp"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP broker is imported for messaging",
                        "AMQP broker is accessed",
                    ),
                ),
                (
                    r'"github\.com/nats-io/nats\.go"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "NATS messaging is imported",
                        "NATS broker is accessed",
                    ),
                ),
                (
                    r"sarama\.NewConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kafka consumer is created for message consumption",
                        "Kafka message is consumed from topic",
                    ),
                ),
                (
                    r"sarama\.NewProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kafka producer is created for message publishing",
                        "Kafka message is produced to topic",
                    ),
                ),
                (
                    r"amqp\.Dial",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP connection is opened to broker",
                        "AMQP broker is accessed",
                    ),
                ),
                (
                    r'"cloud\.google\.com/go/pubsub"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cloud Pub/Sub is imported",
                        "Pub/Sub topic is accessed",
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
                        "WebSocket integration is imported",
                        "WebSocket connection is accessed",
                    ),
                ),
                (
                    r"websocket\.Upgrader",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connection is upgraded from HTTP connection",
                        "WebSocket connections are accepted inbound",
                    ),
                ),
                (
                    r"net\.Listen\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Socket listener is opened",
                        "Network listener is created for inbound connections",
                    ),
                ),
                (
                    r"net\.Dial\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Network connection is opened outbound",
                        "Network endpoint connection is opened outbound",
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
                        "ORM package is imported for database access",
                        "Database is queried via GORM",
                    ),
                ),
                (
                    r'"github\.com/jmoiron/sqlx"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database extensions are imported via sqlx package",
                        "Database is queried via SQL",
                    ),
                ),
                (
                    r'"github\.com/jackc/pgx"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL driver is imported",
                        "PostgreSQL database connection is opened",
                    ),
                ),
                (
                    r'"database/sql"',
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "SQL database interface is imported",
                        "Database is accessed via SQL connection",
                    ),
                ),
                (
                    r"gorm\.Model",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM model is structured for database mapping",
                        "Database model is defined via GORM",
                    ),
                ),
                (
                    r"db\.Create\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database record is created",
                        "Database record is written via GORM",
                    ),
                ),
                (
                    r"db\.Find\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database records are queried",
                        "Database records are queried",
                    ),
                ),
                (
                    r"db\.Where\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database query is filtered with conditions",
                        "Database records are queried filtered",
                    ),
                ),
                (
                    r"sql\.Open\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened with SQL driver",
                        "Database connection is opened for outbound SQL",
                    ),
                ),
                (
                    r'"github\.com/gocql/gocql"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cassandra database is imported",
                        "Cassandra database connection is opened",
                    ),
                ),
                (
                    r'"cloud\.google\.com/go/firestore"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Firestore database is imported",
                        "Firestore database is written",
                    ),
                ),
                (
                    r'"github\.com/neo4j/neo4j-go-driver"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j driver is imported for graph database",
                        "Neo4j database connection is opened",
                    ),
                ),
                (
                    r"neo4j\.NewDriverWithContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j driver is created",
                        "Neo4j database connection is opened",
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
                        "File is opened for reading",
                        "File is read from filesystem",
                    ),
                ),
                (
                    r"os\.Create\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File is created for writing",
                        "File is written to filesystem",
                    ),
                ),
                (
                    r"io\.Copy\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Data is copied between reader and writer",
                        "Data stream is accessed via IO",
                    ),
                ),
                (
                    r"s3manager",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 multipart uploads are managed by upload utility",
                        "S3 bucket is written via AWS SDK",
                    ),
                ),
                (
                    r'"cloud\.google\.com/go/storage"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud storage is imported for bucket access",
                        "Cloud Storage bucket is written",
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
                        "gRPC primitives are imported",
                        "gRPC service is accessed via client",
                    ),
                ),
                (
                    r"pb\.Register.*Server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC service is registered",
                        "gRPC service is exposed for inbound requests",
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
                        "GraphQL server is imported with code generation",
                        "GraphQL API is accessed via client",
                    ),
                ),
                (
                    r'"github\.com/graphql-go/graphql"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL schema is imported for server",
                        "GraphQL API is accessed via client",
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
                        "SMTP email is imported",
                        "Email message is sent via SMTP",
                    ),
                ),
                (
                    r'"gopkg\.in/gomail"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email composer is imported for messaging",
                        "Email message is sent outbound",
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
                        "Redis client is imported for cache integration",
                        "Redis cache is written with data",
                    ),
                ),
                (
                    r'"github\.com/bradfitz/gomemcache"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached client is imported for cache integration",
                        "Memcached cache is written with data",
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
                        "Server-Sent Events are enabled by content type",
                        "Server-Sent Events stream is exposed inbound",
                    ),
                ),
                (
                    r"http\.Flusher",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP data is flushed to streaming client",
                        "Streaming HTTP response is exposed inbound",
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
                        "SFTP client is imported for file transfer",
                        "SFTP server connection is opened outbound",
                    ),
                ),
                (
                    r'"github\.com/jlaffaye/ftp"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP client is imported for file transfer",
                        "FTP server connection is opened outbound",
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
                        "Cron scheduler is imported",
                        "Scheduled job is integrated with cron runner",
                    ),
                ),
                (
                    r"time\.NewTicker",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Periodic timer is created with channel-based ticker",
                        "Periodic scheduler is integrated with timer",
                    ),
                ),
            ],
        },
    },
)
