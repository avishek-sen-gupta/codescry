"""Integration point detection from file contents.

Detects system integration points (HTTP, SOAP, messaging, sockets, database)
from source file contents using regex pattern matching.

Patterns are organized by language to support language-specific conventions.
"""

import re
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Iterator


class IntegrationType(Enum):
    """Types of system integrations that can be detected."""

    HTTP_REST = "http_rest"
    SOAP = "soap"
    MESSAGING = "messaging"
    SOCKET = "socket"
    DATABASE = "database"


class Confidence(Enum):
    """Confidence levels for integration point detection."""

    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


class EntityType(Enum):
    """Type of entity where integration was detected."""

    FILE_CONTENT = "file_content"
    DIRECTORY = "directory"


@dataclass(frozen=True)
class FileMatch:
    """A match found in a file."""

    file_path: str
    line_number: int
    line_content: str
    language: str


@dataclass(frozen=True)
class IntegrationPoint:
    """A detected integration point."""

    match: FileMatch
    integration_type: IntegrationType
    confidence: Confidence
    matched_pattern: str
    entity_type: EntityType


@dataclass(frozen=True)
class IntegrationDetectorResult:
    """Result of integration detection."""

    integration_points: list[IntegrationPoint]
    files_scanned: int


# File extension to language mapping
EXTENSION_TO_LANGUAGE = {
    ".java": "Java",
    ".rs": "Rust",
    ".py": "Python",
    ".ts": "TypeScript",
    ".tsx": "TypeScript",
    ".js": "JavaScript",
    ".jsx": "JavaScript",
    ".go": "Go",
    ".cs": "C#",
    ".kt": "Kotlin",
    ".scala": "Scala",
    ".rb": "Ruby",
    ".php": "PHP",
}

# Common patterns that apply across all languages
COMMON_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"(?i)\bhttp\b", Confidence.LOW),
            (r"(?i)\brest(?:ful)?\b", Confidence.LOW),
            (r"(?i)\bapi\b", Confidence.LOW),
            (r"(?i)\bendpoint\b", Confidence.LOW),
        ],
        "directory_patterns": [
            r"(?i)^controllers?$",
            r"(?i)^api$",
            r"(?i)^rest$",
            r"(?i)^endpoints?$",
            r"(?i)^resources?$",
            r"(?i)^handlers?$",
            r"(?i)^routes?$",
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            (r"(?i)\bsoap\b", Confidence.MEDIUM),
            (r"(?i)\bwsdl\b", Confidence.HIGH),
            (r"(?i)\benvelope\b", Confidence.LOW),
        ],
        "directory_patterns": [
            r"(?i)^soap$",
            r"(?i)^wsdl$",
            r"(?i)^webservices?$",
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"(?i)\bkafka\b", Confidence.HIGH),
            (r"(?i)\brabbit(?:mq)?\b", Confidence.HIGH),
            (r"(?i)\bamqp\b", Confidence.HIGH),
            (r"(?i)\bpulsar\b", Confidence.HIGH),
            (r"(?i)\bnats\b", Confidence.MEDIUM),
        ],
        "directory_patterns": [
            r"(?i)^messaging$",
            r"(?i)^kafka$",
            r"(?i)^rabbit(?:mq)?$",
            r"(?i)^queues?$",
            r"(?i)^events?$",
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"(?i)\bwebsocket\b", Confidence.HIGH),
            (r"(?i)\btcp\b", Confidence.MEDIUM),
            (r"(?i)\budp\b", Confidence.MEDIUM),
        ],
        "directory_patterns": [
            r"(?i)^sockets?$",
            r"(?i)^websockets?$",
        ],
    },
    IntegrationType.DATABASE: {
        "patterns": [
            (r"(?i)\bdatabase\b", Confidence.LOW),
            (r"(?i)\bdatasource\b", Confidence.MEDIUM),
        ],
        "directory_patterns": [
            r"(?i)^repositor(?:y|ies)$",
            r"(?i)^dao$",
            r"(?i)^database$",
            r"(?i)^db$",
            r"(?i)^persistence$",
            r"(?i)^models?$",
        ],
    },
}

# Language-specific patterns with confidence levels
LANGUAGE_PATTERNS = {
    "Java": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@RequestMapping", Confidence.HIGH),
                (r"@GetMapping", Confidence.HIGH),
                (r"@PostMapping", Confidence.HIGH),
                (r"@PutMapping", Confidence.HIGH),
                (r"@DeleteMapping", Confidence.HIGH),
                (r"@PatchMapping", Confidence.HIGH),
                (r"@RestController", Confidence.HIGH),
                (r"@Controller", Confidence.HIGH),
                (r"@RequestBody", Confidence.HIGH),
                (r"@ResponseBody", Confidence.HIGH),
                (r"@PathVariable", Confidence.HIGH),
                (r"@RequestParam", Confidence.HIGH),
                (r"@GET\b", Confidence.HIGH),
                (r"@POST\b", Confidence.HIGH),
                (r"@PUT\b", Confidence.HIGH),
                (r"@DELETE\b", Confidence.HIGH),
                (r"@Path\(", Confidence.HIGH),
                (r"@Produces", Confidence.HIGH),
                (r"@Consumes", Confidence.HIGH),
                (r"HttpServletRequest", Confidence.MEDIUM),
                (r"HttpServletResponse", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            "patterns": [
                (r"@WebService", Confidence.HIGH),
                (r"@WebMethod", Confidence.HIGH),
                (r"@WebParam", Confidence.HIGH),
                (r"@SOAPBinding", Confidence.HIGH),
                (r"SOAPMessage", Confidence.HIGH),
                (r"SOAPEnvelope", Confidence.HIGH),
                (r"JAXBContext", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r"@KafkaListener", Confidence.HIGH),
                (r"@JmsListener", Confidence.HIGH),
                (r"@RabbitListener", Confidence.HIGH),
                (r"@StreamListener", Confidence.HIGH),
                (r"@SendTo", Confidence.HIGH),
                (r"KafkaTemplate", Confidence.HIGH),
                (r"JmsTemplate", Confidence.HIGH),
                (r"RabbitTemplate", Confidence.HIGH),
                (r"MessageListener", Confidence.MEDIUM),
                (r"MessageProducer", Confidence.MEDIUM),
                (r"MessageConsumer", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"@ServerEndpoint", Confidence.HIGH),
                (r"@OnOpen", Confidence.HIGH),
                (r"@OnClose", Confidence.HIGH),
                (r"@OnMessage", Confidence.HIGH),
                (r"@OnError", Confidence.HIGH),
                (r"WebSocketHandler", Confidence.HIGH),
                (r"ServerSocket\b", Confidence.HIGH),
                (r"DatagramSocket", Confidence.HIGH),
                (r"SocketChannel", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r"@Repository", Confidence.HIGH),
                (r"@Entity", Confidence.HIGH),
                (r"@Table", Confidence.HIGH),
                (r"@Query", Confidence.HIGH),
                (r"@Transactional", Confidence.HIGH),
                (r"@PersistenceContext", Confidence.HIGH),
                (r"@Column", Confidence.MEDIUM),
                (r"@JoinColumn", Confidence.MEDIUM),
                (r"@OneToMany", Confidence.MEDIUM),
                (r"@ManyToOne", Confidence.MEDIUM),
                (r"@ManyToMany", Confidence.MEDIUM),
                (r"JdbcTemplate", Confidence.HIGH),
                (r"EntityManager", Confidence.HIGH),
                (r"SessionFactory", Confidence.HIGH),
                (r"PreparedStatement", Confidence.HIGH),
                (r"ResultSet\b", Confidence.MEDIUM),
            ],
        },
    },
    "Rust": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'#\[get\("', Confidence.HIGH),
                (r'#\[post\("', Confidence.HIGH),
                (r'#\[put\("', Confidence.HIGH),
                (r'#\[delete\("', Confidence.HIGH),
                (r'#\[patch\("', Confidence.HIGH),
                (r'#\[route\("', Confidence.HIGH),
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
    },
    "Python": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@app\.route", Confidence.HIGH),
                (r"@app\.get", Confidence.HIGH),
                (r"@app\.post", Confidence.HIGH),
                (r"@app\.put", Confidence.HIGH),
                (r"@app\.delete", Confidence.HIGH),
                (r"@router\.get", Confidence.HIGH),
                (r"@router\.post", Confidence.HIGH),
                (r"@api_view", Confidence.HIGH),
                (r"@action", Confidence.MEDIUM),
                (r"from flask import", Confidence.HIGH),
                (r"from fastapi import", Confidence.HIGH),
                (r"from django\.http import", Confidence.HIGH),
                (r"from rest_framework", Confidence.HIGH),
                (r"from starlette", Confidence.MEDIUM),
                (r"from aiohttp import", Confidence.MEDIUM),
                (r"import requests", Confidence.MEDIUM),
                (r"import httpx", Confidence.MEDIUM),
                (r"APIView", Confidence.HIGH),
                (r"ViewSet", Confidence.HIGH),
                (r"GenericAPIView", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            "patterns": [
                (r"from zeep import", Confidence.HIGH),
                (r"import zeep", Confidence.HIGH),
                (r"from suds", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r"@app\.task", Confidence.HIGH),
                (r"@celery\.task", Confidence.HIGH),
                (r"@shared_task", Confidence.HIGH),
                (r"from celery import", Confidence.HIGH),
                (r"from kombu import", Confidence.HIGH),
                (r"import pika", Confidence.HIGH),
                (r"from kafka import", Confidence.HIGH),
                (r"from aiokafka import", Confidence.HIGH),
                (r"KafkaConsumer", Confidence.HIGH),
                (r"KafkaProducer", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"import websockets", Confidence.HIGH),
                (r"from websockets import", Confidence.HIGH),
                (r"import socketio", Confidence.HIGH),
                (r"from socketio import", Confidence.HIGH),
                (r"@socketio\.on", Confidence.HIGH),
                (r"@sio\.on", Confidence.HIGH),
                (r"import socket\b", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r"from sqlalchemy import", Confidence.HIGH),
                (r"import sqlalchemy", Confidence.HIGH),
                (r"from django\.db import", Confidence.HIGH),
                (r"from peewee import", Confidence.HIGH),
                (r"from tortoise", Confidence.HIGH),
                (r"import psycopg", Confidence.HIGH),
                (r"import pymysql", Confidence.HIGH),
                (r"import asyncpg", Confidence.HIGH),
                (r"@declarative", Confidence.HIGH),
                (r"models\.Model", Confidence.HIGH),
                (r"Column\(", Confidence.MEDIUM),
                (r"relationship\(", Confidence.MEDIUM),
                (r"ForeignKey\(", Confidence.MEDIUM),
            ],
        },
    },
    "TypeScript": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@Controller\(", Confidence.HIGH),
                (r"@Get\(", Confidence.HIGH),
                (r"@Post\(", Confidence.HIGH),
                (r"@Put\(", Confidence.HIGH),
                (r"@Delete\(", Confidence.HIGH),
                (r"@Patch\(", Confidence.HIGH),
                (r"@Body\(", Confidence.HIGH),
                (r"@Param\(", Confidence.HIGH),
                (r"@Query\(", Confidence.HIGH),
                (r"from '@nestjs/common'", Confidence.HIGH),
                (r"from 'express'", Confidence.HIGH),
                (r"from 'fastify'", Confidence.HIGH),
                (r"from 'koa'", Confidence.MEDIUM),
                (r"import axios", Confidence.MEDIUM),
                (r"app\.get\(", Confidence.HIGH),
                (r"app\.post\(", Confidence.HIGH),
                (r"router\.get\(", Confidence.HIGH),
                (r"router\.post\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            "patterns": [
                (r"import.*soap", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r"@MessagePattern\(", Confidence.HIGH),
                (r"@EventPattern\(", Confidence.HIGH),
                (r"from 'kafkajs'", Confidence.HIGH),
                (r"from 'amqplib'", Confidence.HIGH),
                (r"from 'bull'", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"@WebSocketGateway", Confidence.HIGH),
                (r"@SubscribeMessage", Confidence.HIGH),
                (r"from 'socket.io'", Confidence.HIGH),
                (r"from 'ws'", Confidence.HIGH),
                (r"io\.on\(", Confidence.HIGH),
                (r"socket\.on\(", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r"@Entity\(", Confidence.HIGH),
                (r"@Column\(", Confidence.MEDIUM),
                (r"@PrimaryGeneratedColumn", Confidence.HIGH),
                (r"@ManyToOne", Confidence.MEDIUM),
                (r"@OneToMany", Confidence.MEDIUM),
                (r"@Repository\(", Confidence.HIGH),
                (r"from 'typeorm'", Confidence.HIGH),
                (r"from '@prisma/client'", Confidence.HIGH),
                (r"from 'sequelize'", Confidence.HIGH),
                (r"from 'mongoose'", Confidence.HIGH),
                (r"prisma\.", Confidence.HIGH),
            ],
        },
    },
    "JavaScript": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"require\(['\"]express['\"]\)", Confidence.HIGH),
                (r"require\(['\"]fastify['\"]\)", Confidence.HIGH),
                (r"require\(['\"]koa['\"]\)", Confidence.MEDIUM),
                (r"require\(['\"]axios['\"]\)", Confidence.MEDIUM),
                (r"app\.get\(", Confidence.HIGH),
                (r"app\.post\(", Confidence.HIGH),
                (r"app\.put\(", Confidence.HIGH),
                (r"app\.delete\(", Confidence.HIGH),
                (r"router\.get\(", Confidence.HIGH),
                (r"router\.post\(", Confidence.HIGH),
                (r"fetch\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            "patterns": [
                (r"require\(['\"]soap['\"]\)", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r"require\(['\"]kafkajs['\"]\)", Confidence.HIGH),
                (r"require\(['\"]amqplib['\"]\)", Confidence.HIGH),
                (r"require\(['\"]bull['\"]\)", Confidence.HIGH),
                (r"consumer\.run\(", Confidence.HIGH),
                (r"producer\.send\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"require\(['\"]socket.io['\"]\)", Confidence.HIGH),
                (r"require\(['\"]ws['\"]\)", Confidence.HIGH),
                (r"io\.on\(", Confidence.HIGH),
                (r"socket\.on\(", Confidence.HIGH),
                (r"new WebSocket\(", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r"require\(['\"]sequelize['\"]\)", Confidence.HIGH),
                (r"require\(['\"]mongoose['\"]\)", Confidence.HIGH),
                (r"require\(['\"]knex['\"]\)", Confidence.HIGH),
                (r"require\(['\"]mongodb['\"]\)", Confidence.HIGH),
                (r"mongoose\.Schema", Confidence.HIGH),
                (r"mongoose\.model\(", Confidence.HIGH),
                (r"Sequelize\.define\(", Confidence.HIGH),
            ],
        },
    },
    "Go": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/gin-gonic/gin"', Confidence.HIGH),
                (r'"github\.com/labstack/echo"', Confidence.HIGH),
                (r'"github\.com/gofiber/fiber"', Confidence.HIGH),
                (r'"github\.com/go-chi/chi"', Confidence.HIGH),
                (r'"github\.com/gorilla/mux"', Confidence.HIGH),
                (r'"net/http"', Confidence.MEDIUM),
                (r"http\.HandleFunc", Confidence.HIGH),
                (r"http\.Handle\(", Confidence.HIGH),
                (r"gin\.Context", Confidence.HIGH),
                (r"echo\.Context", Confidence.HIGH),
                (r"fiber\.Ctx", Confidence.HIGH),
                (r"\.GET\(", Confidence.HIGH),
                (r"\.POST\(", Confidence.HIGH),
                (r"\.PUT\(", Confidence.HIGH),
                (r"\.DELETE\(", Confidence.HIGH),
                (r"r\.HandleFunc\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            "patterns": [],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r'"github\.com/Shopify/sarama"', Confidence.HIGH),
                (r'"github\.com/streadway/amqp"', Confidence.HIGH),
                (r'"github\.com/nats-io/nats\.go"', Confidence.HIGH),
                (r"sarama\.NewConsumer", Confidence.HIGH),
                (r"sarama\.NewProducer", Confidence.HIGH),
                (r"amqp\.Dial", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r'"github\.com/gorilla/websocket"', Confidence.HIGH),
                (r"websocket\.Upgrader", Confidence.HIGH),
                (r"net\.Listen\(", Confidence.MEDIUM),
                (r"net\.Dial\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r'"gorm\.io/gorm"', Confidence.HIGH),
                (r'"github\.com/jmoiron/sqlx"', Confidence.HIGH),
                (r'"github\.com/jackc/pgx"', Confidence.HIGH),
                (r'"database/sql"', Confidence.MEDIUM),
                (r"gorm\.Model", Confidence.HIGH),
                (r"db\.Create\(", Confidence.HIGH),
                (r"db\.Find\(", Confidence.HIGH),
                (r"db\.Where\(", Confidence.HIGH),
                (r"sql\.Open\(", Confidence.HIGH),
            ],
        },
    },
    "C#": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"\[ApiController\]", Confidence.HIGH),
                (r"\[HttpGet\]", Confidence.HIGH),
                (r"\[HttpPost\]", Confidence.HIGH),
                (r"\[HttpPut\]", Confidence.HIGH),
                (r"\[HttpDelete\]", Confidence.HIGH),
                (r"\[Route\(", Confidence.HIGH),
                (r"\[FromBody\]", Confidence.HIGH),
                (r"\[FromQuery\]", Confidence.HIGH),
                (r"ControllerBase", Confidence.HIGH),
                (r"IActionResult", Confidence.MEDIUM),
                (r"ActionResult<", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            "patterns": [
                (r"\[ServiceContract\]", Confidence.HIGH),
                (r"\[OperationContract\]", Confidence.HIGH),
                (r"\[DataContract\]", Confidence.HIGH),
                (r"\[DataMember\]", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r"using MassTransit", Confidence.HIGH),
                (r"using NServiceBus", Confidence.HIGH),
                (r"using Rebus", Confidence.HIGH),
                (r"IConsumer<", Confidence.HIGH),
                (r"IMessageHandler", Confidence.HIGH),
                (r"IBus\b", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"using Microsoft\.AspNetCore\.SignalR", Confidence.HIGH),
                (r": Hub\b", Confidence.HIGH),
                (r"HubConnection", Confidence.HIGH),
                (r"TcpClient", Confidence.MEDIUM),
                (r"TcpListener", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r"using.*EntityFramework", Confidence.HIGH),
                (r": DbContext", Confidence.HIGH),
                (r"DbSet<", Confidence.HIGH),
                (r"\[Table\(", Confidence.HIGH),
                (r"\[Key\]", Confidence.MEDIUM),
                (r"\[Column\(", Confidence.MEDIUM),
                (r"SqlConnection", Confidence.HIGH),
                (r"using Dapper", Confidence.HIGH),
            ],
        },
    },
}


def get_language_from_extension(file_path: str) -> str:
    """Determine programming language from file extension.

    Args:
        file_path: Path to the file.

    Returns:
        Language name or empty string if unknown.
    """
    ext = Path(file_path).suffix.lower()
    return EXTENSION_TO_LANGUAGE.get(ext, "")


def get_patterns_for_language(
    language: str,
) -> dict[IntegrationType, list[tuple[str, Confidence]]]:
    """Get integration patterns for a specific language.

    Args:
        language: The programming language.

    Returns:
        Dict mapping IntegrationType to list of (pattern, confidence) tuples.
    """
    result: dict[IntegrationType, list[tuple[str, Confidence]]] = {}

    for integration_type in IntegrationType:
        patterns: list[tuple[str, Confidence]] = []

        # Add common patterns
        common = COMMON_PATTERNS.get(integration_type, {})
        patterns.extend(common.get("patterns", []))

        # Add language-specific patterns
        lang_patterns = LANGUAGE_PATTERNS.get(language, {})
        type_patterns = lang_patterns.get(integration_type, {})
        patterns.extend(type_patterns.get("patterns", []))

        result[integration_type] = patterns

    return result


def scan_file_for_integrations(
    file_path: Path,
) -> Iterator[IntegrationPoint]:
    """Scan a single file for integration points.

    Args:
        file_path: Path to the file to scan.

    Yields:
        IntegrationPoint instances for each match found.
    """
    try:
        content = file_path.read_text(encoding="utf-8", errors="ignore")
    except (OSError, IOError):
        return

    language = get_language_from_extension(str(file_path))
    patterns = get_patterns_for_language(language)

    lines = content.splitlines()

    for line_num, line in enumerate(lines, start=1):
        for integration_type, type_patterns in patterns.items():
            for pattern, confidence in type_patterns:
                if re.search(pattern, line):
                    match = FileMatch(
                        file_path=str(file_path),
                        line_number=line_num,
                        line_content=line.strip(),
                        language=language,
                    )
                    yield IntegrationPoint(
                        match=match,
                        integration_type=integration_type,
                        confidence=confidence,
                        matched_pattern=pattern,
                        entity_type=EntityType.FILE_CONTENT,
                    )


def classify_directory(
    directory_name: str,
) -> list[tuple[IntegrationType, Confidence, str]]:
    """Classify a directory name into integration types.

    Args:
        directory_name: The directory name to classify.

    Returns:
        List of (integration_type, confidence, matched_pattern) tuples.
    """
    matches: list[tuple[IntegrationType, Confidence, str]] = []

    for integration_type, patterns in COMMON_PATTERNS.items():
        directory_patterns = patterns.get("directory_patterns", [])
        for pattern in directory_patterns:
            if re.search(pattern, directory_name):
                matches.append(
                    (integration_type, Confidence.MEDIUM, f"directory:{pattern}")
                )
                break

    return matches


def _get_source_files(
    repo_path: Path,
    languages: list[str] | None = None,
) -> Iterator[Path]:
    """Get source files from a repository.

    Args:
        repo_path: Path to the repository.
        languages: Optional list of languages to filter by.

    Yields:
        Paths to source files.
    """
    # Directories to skip
    skip_dirs = {
        ".git",
        ".idea",
        ".vscode",
        "node_modules",
        "__pycache__",
        ".venv",
        "venv",
        "target",
        "build",
        "dist",
        ".tox",
        ".pytest_cache",
        ".mypy_cache",
    }

    # Get allowed extensions based on languages
    if languages:
        allowed_extensions = {
            ext
            for ext, lang in EXTENSION_TO_LANGUAGE.items()
            if lang in languages
        }
    else:
        allowed_extensions = set(EXTENSION_TO_LANGUAGE.keys())

    for path in repo_path.rglob("*"):
        if path.is_file():
            # Skip files in excluded directories
            if any(skip_dir in path.parts for skip_dir in skip_dirs):
                continue

            # Check extension
            if path.suffix.lower() in allowed_extensions:
                yield path


def detect_integrations(
    repo_path: str | Path,
    languages: list[str] | None = None,
) -> IntegrationDetectorResult:
    """Detect integration points from repository file contents.

    Scans source files for patterns indicating system integrations.

    Args:
        repo_path: Path to the repository to scan.
        languages: Optional list of languages to scan (e.g., ["Rust", "Python"]).
                   If None, scans all supported languages.

    Returns:
        IntegrationDetectorResult with detected integration points.
    """
    repo_path = Path(repo_path)

    if not repo_path.is_dir():
        return IntegrationDetectorResult(
            integration_points=[],
            files_scanned=0,
        )

    integration_points: list[IntegrationPoint] = []
    files_scanned = 0

    # Scan source files
    for file_path in _get_source_files(repo_path, languages):
        files_scanned += 1
        integration_points.extend(scan_file_for_integrations(file_path))

    # Scan directory names
    scanned_dirs: set[str] = set()
    for file_path in _get_source_files(repo_path, languages):
        for parent in file_path.relative_to(repo_path).parents:
            dir_name = parent.name
            if dir_name and dir_name not in scanned_dirs:
                scanned_dirs.add(dir_name)
                for int_type, confidence, pattern in classify_directory(dir_name):
                    # Create a placeholder match for the directory
                    match = FileMatch(
                        file_path=str(repo_path / parent),
                        line_number=0,
                        line_content="",
                        language="",
                    )
                    integration_points.append(
                        IntegrationPoint(
                            match=match,
                            integration_type=int_type,
                            confidence=confidence,
                            matched_pattern=pattern,
                            entity_type=EntityType.DIRECTORY,
                        )
                    )

    return IntegrationDetectorResult(
        integration_points=integration_points,
        files_scanned=files_scanned,
    )
