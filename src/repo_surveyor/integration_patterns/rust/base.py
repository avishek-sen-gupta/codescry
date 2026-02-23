"""Rust base integration patterns."""

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
                    r"Json<",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "JSON request is extracted from HTTP",
                        "HTTP JSON data is processed",
                    ),
                ),
                (
                    r"Path<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP path parameters are extracted",
                        "HTTP route parameters are processed",
                    ),
                ),
                (
                    r"Query<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP query parameters are extracted",
                        "HTTP query parameters are processed",
                    ),
                ),
                (
                    r"State<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Application state is extracted for HTTP handlers",
                        "HTTP application state is shared",
                    ),
                ),
                (
                    r"use hyper::",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP communication is imported for client server",
                        "HTTP endpoint is accessed via hyper client",
                    ),
                ),
                (
                    r"use reqwest::",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported for outbound requests",
                        "HTTP request is sent via reqwest client",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"use yaserde::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML serialization is enabled for SOAP messages",
                        "XML data is processed via yaserde",
                    ),
                ),
                (
                    r"use quick_xml::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "XML processing is imported for parsing writing",
                        "XML data is processed via quick_xml parser",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"use rdkafka::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kafka client is imported for messaging",
                        "Kafka broker is accessed via rdkafka client",
                    ),
                ),
                (
                    r"use lapin::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP messaging is imported for RabbitMQ",
                        "AMQP broker is accessed via lapin client",
                    ),
                ),
                (
                    r"StreamConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kafka messages are consumed by StreamConsumer",
                        "Kafka message is consumed via rdkafka consumer",
                    ),
                ),
                (
                    r"FutureProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kafka messages are produced by FutureProducer",
                        "Kafka message is produced via rdkafka producer",
                    ),
                ),
                (
                    r"BaseConsumer",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Kafka messages are consumed by BaseConsumer",
                        "Kafka message is consumed via rdkafka consumer",
                    ),
                ),
                (
                    r"BaseProducer",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Kafka messages are produced by BaseProducer",
                        "Kafka message is produced via rdkafka producer",
                    ),
                ),
                (
                    r"aws-sdk-sqs",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SQS client is imported for queue messaging",
                        "SQS queue is accessed for messaging",
                    ),
                ),
                (
                    r"aws-sdk-sns",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SNS client is imported for pub/sub notifications",
                        "SNS notification is published via SDK",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"TcpListener::",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TCP connections are accepted by listener",
                        "TCP connection is accepted for inbound requests",
                    ),
                ),
                (
                    r"TcpStream::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP communication is handled by stream",
                        "TCP socket stream is accessed",
                    ),
                ),
                (
                    r"UdpSocket::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP communication is handled by socket",
                        "UDP socket is accessed",
                    ),
                ),
                (
                    r"use tokio::net::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Async network I/O is imported",
                        "Network socket is accessed asynchronously",
                    ),
                ),
                (
                    r"use async_std::net::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Async network I/O is imported",
                        "Network socket is accessed asynchronously",
                    ),
                ),
                (
                    r"use tungstenite::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WebSocket communication is enabled by library import",
                        "WebSocket connection is accessed via tungstenite",
                    ),
                ),
                (
                    r"WebSocketStream",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connections are accepted by stream",
                        "WebSocket connection is accepted for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"use diesel::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM library is imported",
                        "Database is accessed via ORM",
                    ),
                ),
                (
                    r"use sqlx::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL database is imported for async access",
                        "Database is queried via sqlx interface",
                    ),
                ),
                (
                    r"use sea_orm::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Async ORM is imported for database access",
                        "Database is accessed via ORM",
                    ),
                ),
                (
                    r"use tokio_postgres::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL client is imported for async access",
                        "PostgreSQL database connection is opened via tokio-postgres",
                    ),
                ),
                (
                    r"use rusqlite::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQLite database is imported",
                        "SQLite database connection is opened via rusqlite",
                    ),
                ),
                (
                    r"#\[derive\(.*Queryable",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database query mapping is derived with macro",
                        "Database is queried via ORM",
                    ),
                ),
                (
                    r"#\[derive\(.*Insertable",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database insert mapping is derived with macro",
                        "Database is written via ORM",
                    ),
                ),
                (
                    r"#\[table_name",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is mapped with struct attribute",
                        "Database table is accessed via ORM",
                    ),
                ),
                (
                    r"#\[diesel\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM mapping is configured with attribute",
                        "Database is accessed via ORM",
                    ),
                ),
                (
                    r"PgConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL connection is typed for database access",
                        "PostgreSQL database is accessed via ORM",
                    ),
                ),
                (
                    r"SqliteConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQLite connection is typed for database access",
                        "SQLite database is accessed via ORM",
                    ),
                ),
                (
                    r"MysqlConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL connection is typed for database access",
                        "MySQL database is accessed via ORM",
                    ),
                ),
                (
                    r"sqlx::query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed with macro",
                        "Database is queried via sqlx interface",
                    ),
                ),
                (
                    r"query_as!",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed with typed macro",
                        "Database is queried via sqlx interface",
                    ),
                ),
                (
                    r"query!",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL query is executed with checked macro",
                        "Database is queried via sqlx interface",
                    ),
                ),
                (
                    r"aws-sdk-dynamodb",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "DynamoDB client is imported for NoSQL access",
                        "DynamoDB database is accessed via SDK",
                    ),
                ),
                (
                    r"use neo4rs::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j client is imported for graph database",
                        "Neo4j graph database is accessed via neo4rs",
                    ),
                ),
                (
                    r"neo4rs::Graph::new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j connection is created with Graph",
                        "Neo4j database connection is opened via neo4rs",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"std::fs",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File system is accessed synchronously",
                        "File system is accessed via std::fs",
                    ),
                ),
                (
                    r"tokio::fs",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File system is accessed with async operations",
                        "File system is accessed asynchronously",
                    ),
                ),
                (
                    r"aws-sdk-s3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 client is imported for object storage",
                        "S3 object storage is accessed via SDK",
                    ),
                ),
                (
                    r"azure_storage_blobs",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Azure Blob Storage is accessed by client",
                        "Azure Blob Storage is accessed via SDK",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"tonic::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "gRPC client is created for RPC communication",
                        "gRPC endpoint is accessed via tonic client",
                    ),
                ),
                (
                    r"#\[tonic::async_trait\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC service is implemented with async trait",
                        "gRPC service is exposed via tonic",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"use juniper::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL server is imported",
                        "GraphQL API is accessed via juniper interface",
                    ),
                ),
                (
                    r"use async_graphql::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL server is imported for async queries",
                        "GraphQL API is accessed via client",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"use lettre::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email sending is imported",
                        "Email message is sent via lettre client",
                    ),
                ),
                (
                    r"SmtpTransport",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SMTP email is sent by transport",
                        "Email is sent via SMTP using lettre",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"use redis::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is imported",
                        "Redis cache connection is opened via redis",
                    ),
                ),
                (
                    r"use deadpool_redis::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis connection pool is imported for async access",
                        "Redis cache is accessed via connection pool",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"use eventsource::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Server-sent events are imported",
                        "SSE stream is accessed for events",
                    ),
                ),
                (
                    r"Sse<",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-sent events are exposed by SSE stream",
                        "SSE endpoint is exposed for streaming events",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"use ssh2::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SSH client is imported for SFTP",
                        "SSH server connection is opened via ssh2",
                    ),
                ),
                (
                    r"Sftp::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP operations are handled by type",
                        "SFTP server connection is opened via ssh2",
                    ),
                ),
                (
                    r"use suppaftp::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP client is imported",
                        "FTP server connection is opened via suppaftp",
                    ),
                ),
                (
                    r"FtpStream::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP connection is handled by stream",
                        "FTP server connection is opened via suppaftp",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"use tokio_cron_scheduler::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cron scheduling is imported for task execution",
                        "Scheduled tasks are managed via tokio-cron-scheduler",
                    ),
                ),
                (
                    r"use cron::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cron scheduling is imported for task execution",
                        "Scheduled tasks are managed by cron",
                    ),
                ),
            ],
        },
    },
)
