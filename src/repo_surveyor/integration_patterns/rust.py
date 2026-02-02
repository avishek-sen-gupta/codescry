"""Rust integration patterns."""

from .types import Confidence, IntegrationType

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"#\[get\(", Confidence.HIGH),
            (r"#\[post\(", Confidence.HIGH),
            (r"#\[put\(", Confidence.HIGH),
            (r"#\[delete\(", Confidence.HIGH),
            (r"#\[patch\(", Confidence.HIGH),
            (r"#\[route\(", Confidence.HIGH),
            (r"HttpResponse", Confidence.HIGH),
            (r"HttpRequest", Confidence.HIGH),
            (r"web::Json", Confidence.HIGH),
            (r"web::Path", Confidence.HIGH),
            (r"web::Query", Confidence.HIGH),
            (r"web::Data", Confidence.HIGH),
            (r"Json<", Confidence.HIGH),
            (r"Path<", Confidence.MEDIUM),
            (r"Query<", Confidence.MEDIUM),
            (r"State<", Confidence.MEDIUM),
            (r"Router::", Confidence.MEDIUM),
            (r"\.route\(", Confidence.MEDIUM),
            (r"use actix_web::", Confidence.HIGH),
            (r"use axum::", Confidence.HIGH),
            (r"use rocket::", Confidence.HIGH),
            (r"use warp::", Confidence.HIGH),
            (r"use hyper::", Confidence.MEDIUM),
            (r"use reqwest::", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"use rdkafka::", Confidence.HIGH),
            (r"use lapin::", Confidence.HIGH),
            (r"StreamConsumer", Confidence.HIGH),
            (r"FutureProducer", Confidence.HIGH),
            (r"BaseConsumer", Confidence.MEDIUM),
            (r"BaseProducer", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
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
        "patterns": [
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
}
