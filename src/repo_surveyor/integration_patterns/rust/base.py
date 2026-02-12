"""Rust base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"Json<", Confidence.HIGH),
                (r"Path<", Confidence.MEDIUM),
                (r"Query<", Confidence.MEDIUM),
                (r"State<", Confidence.MEDIUM),
                (r"use hyper::", Confidence.MEDIUM),
                (r"use reqwest::", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"use yaserde::", Confidence.HIGH),
                (r"use quick_xml::", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"use rdkafka::", Confidence.HIGH),
                (r"use lapin::", Confidence.HIGH),
                (r"StreamConsumer", Confidence.HIGH),
                (r"FutureProducer", Confidence.HIGH),
                (r"BaseConsumer", Confidence.MEDIUM),
                (r"BaseProducer", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"TcpListener::", Confidence.HIGH),
                (r"TcpStream::", Confidence.HIGH),
                (r"UdpSocket::", Confidence.HIGH),
                (r"use tokio::net::", Confidence.HIGH),
                (r"use async_std::net::", Confidence.HIGH),
                (r"use tungstenite::", Confidence.HIGH),
                (r"WebSocketStream", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"use diesel::", Confidence.HIGH),
                (r"use sqlx::", Confidence.HIGH),
                (r"use sea_orm::", Confidence.HIGH),
                (r"use tokio_postgres::", Confidence.HIGH),
                (r"use rusqlite::", Confidence.HIGH),
                (r"#\[derive\(.*Queryable", Confidence.HIGH),
                (r"#\[derive\(.*Insertable", Confidence.HIGH),
                (r"#\[table_name", Confidence.HIGH),
                (r"#\[diesel\(", Confidence.HIGH),
                (r"PgConnection", Confidence.HIGH),
                (r"SqliteConnection", Confidence.HIGH),
                (r"MysqlConnection", Confidence.HIGH),
                (r"sqlx::query", Confidence.HIGH),
                (r"query_as!", Confidence.HIGH),
                (r"query!", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"std::fs", Confidence.HIGH),
                (r"tokio::fs", Confidence.HIGH),
                (r"aws-sdk-s3", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"tonic::", Confidence.HIGH),
                (r"#\[tonic::async_trait\]", Confidence.HIGH),
            ],
        },
    },
)
