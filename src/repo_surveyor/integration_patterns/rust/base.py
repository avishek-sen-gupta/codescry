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
                        "Rust Json extractor handling HTTP JSON request or response",
                        "This code uses Rust to interact with HTTP JSON data",
                    ),
                ),
                (
                    r"Path<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust Path extractor handling HTTP route path parameters",
                        "This code uses Rust to interact with HTTP route parameters",
                    ),
                ),
                (
                    r"Query<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust Query extractor handling HTTP query string parameters",
                        "This code uses Rust to interact with HTTP query parameters",
                    ),
                ),
                (
                    r"State<",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust State extractor sharing application state in HTTP handlers",
                        "This code uses Rust to interact with shared HTTP application state",
                    ),
                ),
                (
                    r"use hyper::",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust hyper library import for HTTP client and server",
                        "This code uses Rust hyper to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"use reqwest::",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rust reqwest HTTP client library import",
                        "This code uses Rust reqwest to make outbound HTTP requests",
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
                        "Rust yaserde library import for XML/SOAP serialization",
                        "This code uses Rust yaserde to interact with XML/SOAP data",
                    ),
                ),
                (
                    r"use quick_xml::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust quick_xml library import for XML parsing and writing",
                        "This code uses Rust quick_xml to interact with XML/SOAP data",
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
                        "Rust rdkafka library import for Kafka messaging client",
                        "This code uses Rust rdkafka to interact with a Kafka broker",
                    ),
                ),
                (
                    r"use lapin::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust lapin library import for AMQP/RabbitMQ messaging client",
                        "This code uses Rust lapin to interact with an AMQP broker",
                    ),
                ),
                (
                    r"StreamConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rust Kafka StreamConsumer type consuming inbound Kafka messages",
                        "This code uses Rust rdkafka to receive inbound Kafka messages",
                    ),
                ),
                (
                    r"FutureProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Kafka FutureProducer type producing outbound Kafka messages",
                        "This code uses Rust rdkafka to produce outbound Kafka messages",
                    ),
                ),
                (
                    r"BaseConsumer",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Rust Kafka BaseConsumer type consuming inbound Kafka messages",
                        "This code uses Rust rdkafka to receive inbound Kafka messages",
                    ),
                ),
                (
                    r"BaseProducer",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Kafka BaseProducer type producing outbound Kafka messages",
                        "This code uses Rust rdkafka to produce outbound Kafka messages",
                    ),
                ),
                (
                    r"aws-sdk-sqs",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust AWS SDK SQS crate for queue messaging",
                        "This code uses Rust AWS SDK to interact with an SQS queue",
                    ),
                ),
                (
                    r"aws-sdk-sns",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust AWS SDK SNS crate for pub/sub notifications",
                        "This code uses Rust AWS SDK to produce outbound SNS notifications",
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
                        "Rust TcpListener type accepting inbound TCP connections",
                        "This code uses Rust to accept inbound TCP connections",
                    ),
                ),
                (
                    r"TcpStream::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust TcpStream type for TCP stream communication",
                        "This code uses Rust to interact with a TCP stream socket",
                    ),
                ),
                (
                    r"UdpSocket::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust UdpSocket type for UDP socket communication",
                        "This code uses Rust to interact with a UDP socket",
                    ),
                ),
                (
                    r"use tokio::net::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust Tokio net module import for async network I/O",
                        "This code uses Rust Tokio to interact with async network sockets",
                    ),
                ),
                (
                    r"use async_std::net::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust async-std net module import for async network I/O",
                        "This code uses Rust async-std to interact with async network sockets",
                    ),
                ),
                (
                    r"use tungstenite::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust tungstenite library import for WebSocket communication",
                        "This code uses Rust tungstenite to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"WebSocketStream",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rust WebSocketStream type accepting inbound WebSocket connections",
                        "This code uses Rust to accept inbound WebSocket connections",
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
                        "Rust Diesel ORM library import",
                        "This code uses Rust Diesel ORM to interact with a relational database",
                    ),
                ),
                (
                    r"use sqlx::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust sqlx library import for async SQL database access",
                        "This code uses Rust sqlx to query a relational database",
                    ),
                ),
                (
                    r"use sea_orm::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust SeaORM library import for async ORM database access",
                        "This code uses Rust SeaORM to interact with a relational database",
                    ),
                ),
                (
                    r"use tokio_postgres::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust tokio-postgres library import for async PostgreSQL client",
                        "This code uses Rust tokio-postgres to connect to a PostgreSQL database",
                    ),
                ),
                (
                    r"use rusqlite::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust rusqlite library import for SQLite database access",
                        "This code uses Rust rusqlite to connect to a SQLite database",
                    ),
                ),
                (
                    r"#\[derive\(.*Queryable",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Diesel Queryable derive macro for database query result mapping",
                        "This code uses Rust Diesel ORM to query a relational database",
                    ),
                ),
                (
                    r"#\[derive\(.*Insertable",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Diesel Insertable derive macro for database insert mapping",
                        "This code uses Rust Diesel ORM to write to a relational database",
                    ),
                ),
                (
                    r"#\[table_name",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Diesel table_name attribute mapping a struct to a database table",
                        "This code uses Rust Diesel ORM to interact with a relational database table",
                    ),
                ),
                (
                    r"#\[diesel\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Diesel attribute macro configuring ORM database mapping",
                        "This code uses Rust Diesel ORM to interact with a relational database",
                    ),
                ),
                (
                    r"PgConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Diesel PgConnection type for PostgreSQL database connection",
                        "This code uses Rust Diesel ORM to connect to a PostgreSQL database",
                    ),
                ),
                (
                    r"SqliteConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Diesel SqliteConnection type for SQLite database connection",
                        "This code uses Rust Diesel ORM to connect to a SQLite database",
                    ),
                ),
                (
                    r"MysqlConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust Diesel MysqlConnection type for MySQL database connection",
                        "This code uses Rust Diesel ORM to connect to a MySQL database",
                    ),
                ),
                (
                    r"sqlx::query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust sqlx::query macro executing a SQL query",
                        "This code uses Rust sqlx to query a relational database",
                    ),
                ),
                (
                    r"query_as!",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust sqlx query_as! macro executing a typed SQL query",
                        "This code uses Rust sqlx to query a relational database",
                    ),
                ),
                (
                    r"query!",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust sqlx query! macro executing a checked SQL query",
                        "This code uses Rust sqlx to query a relational database",
                    ),
                ),
                (
                    r"aws-sdk-dynamodb",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust AWS SDK DynamoDB crate for NoSQL database access",
                        "This code uses Rust AWS SDK to interact with a DynamoDB database",
                    ),
                ),
                (
                    r"use neo4rs::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust neo4rs library import for Neo4j graph database client",
                        "This code uses Rust neo4rs to interact with a Neo4j graph database",
                    ),
                ),
                (
                    r"neo4rs::Graph::new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust neo4rs Graph::new call creating a Neo4j database connection",
                        "This code uses Rust neo4rs to connect to a Neo4j graph database",
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
                        "Rust std::fs module for synchronous file system operations",
                        "This code uses Rust std::fs to interact with the file system",
                    ),
                ),
                (
                    r"tokio::fs",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust tokio::fs module for async file system operations",
                        "This code uses Rust Tokio to interact with the file system asynchronously",
                    ),
                ),
                (
                    r"aws-sdk-s3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust AWS SDK S3 crate for object storage access",
                        "This code uses Rust AWS SDK to interact with S3 object storage",
                    ),
                ),
                (
                    r"azure_storage_blobs",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust azure_storage_blobs crate for Azure Blob Storage access",
                        "This code uses Rust Azure SDK to interact with Azure Blob Storage",
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
                        "Rust tonic library for gRPC client and server",
                        "This code uses Rust tonic to interact with a gRPC endpoint",
                    ),
                ),
                (
                    r"#\[tonic::async_trait\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rust tonic::async_trait attribute implementing a gRPC service",
                        "This code uses Rust tonic to expose an inbound gRPC service",
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
                        "Rust juniper library import for GraphQL server",
                        "This code uses Rust juniper to interact with a GraphQL API",
                    ),
                ),
                (
                    r"use async_graphql::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust async-graphql library import for async GraphQL server",
                        "This code uses Rust async-graphql to interact with a GraphQL API",
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
                        "Rust lettre library import for email sending",
                        "This code uses Rust lettre to send outbound email messages",
                    ),
                ),
                (
                    r"SmtpTransport",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust lettre SmtpTransport type for sending email via SMTP",
                        "This code uses Rust lettre to send outbound email via SMTP",
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
                        "Rust redis library import for Redis cache client",
                        "This code uses Rust redis to connect to a Redis cache",
                    ),
                ),
                (
                    r"use deadpool_redis::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust deadpool-redis library import for async Redis connection pool",
                        "This code uses Rust deadpool-redis to connect to a Redis cache",
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
                        "Rust eventsource library import for server-sent events",
                        "This code uses Rust eventsource to interact with SSE streaming",
                    ),
                ),
                (
                    r"Sse<",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Rust Sse type exposing an inbound server-sent events stream",
                        "This code uses Rust to expose an inbound SSE streaming endpoint",
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
                        "Rust ssh2 library import for SSH and SFTP client",
                        "This code uses Rust ssh2 to connect to an SSH/SFTP server",
                    ),
                ),
                (
                    r"Sftp::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust ssh2 Sftp type for SFTP file transfer operations",
                        "This code uses Rust ssh2 to connect to an SFTP server",
                    ),
                ),
                (
                    r"use suppaftp::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust suppaftp library import for FTP/FTPS client",
                        "This code uses Rust suppaftp to connect to an FTP server",
                    ),
                ),
                (
                    r"FtpStream::",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Rust suppaftp FtpStream type for FTP connection and transfer",
                        "This code uses Rust suppaftp to connect to an FTP server",
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
                        "Rust tokio-cron-scheduler library import for cron task scheduling",
                        "This code uses Rust tokio-cron-scheduler to interact with scheduled tasks",
                    ),
                ),
                (
                    r"use cron::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Rust cron library import for cron expression parsing and scheduling",
                        "This code uses Rust cron to interact with scheduled tasks",
                    ),
                ),
            ],
        },
    },
)
