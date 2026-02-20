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
                (r"Json<", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Path<", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"Query<", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"State<", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"use hyper::", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"use reqwest::", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"use yaserde::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"use quick_xml::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"use rdkafka::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"use lapin::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"StreamConsumer", Confidence.HIGH, SignalDirection.INWARD),
                (r"FutureProducer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"BaseConsumer", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"BaseProducer", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"aws-sdk-sqs", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"aws-sdk-sns", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"TcpListener::", Confidence.HIGH, SignalDirection.INWARD),
                (r"TcpStream::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"UdpSocket::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"use tokio::net::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"use async_std::net::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"use tungstenite::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"WebSocketStream", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"use diesel::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"use sqlx::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"use sea_orm::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"use tokio_postgres::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"use rusqlite::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"#\[derive\(.*Queryable", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"#\[derive\(.*Insertable", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"#\[table_name", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"#\[diesel\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PgConnection", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SqliteConnection", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MysqlConnection", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"sqlx::query", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"query_as!", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"query!", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"aws-sdk-dynamodb", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"use neo4rs::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"neo4rs::Graph::new", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"std::fs", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"tokio::fs", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"aws-sdk-s3", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"azure_storage_blobs", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"tonic::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"#\[tonic::async_trait\]", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"use juniper::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"use async_graphql::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"use lettre::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SmtpTransport", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"use redis::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"use deadpool_redis::", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"use eventsource::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Sse<", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"use ssh2::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Sftp::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"use suppaftp::", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"FtpStream::", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"use tokio_cron_scheduler::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"use cron::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
