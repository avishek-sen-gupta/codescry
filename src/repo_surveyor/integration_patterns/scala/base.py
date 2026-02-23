"""Scala base integration patterns."""

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
                    r"import sttp\.client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported via sttp",
                        "HTTP request is sent with sttp client",
                    ),
                ),
                (
                    r"import scalaj\.http",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported via scalaj-http",
                        "HTTP request is sent with scalaj client",
                    ),
                ),
                (
                    r"import dispatch\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP requests are sent with async client",
                        "HTTP request is sent via Dispatch",
                    ),
                ),
                (
                    r"HttpURLConnection",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP connection is opened for client requests",
                        "HTTP request is sent outbound",
                    ),
                ),
                (
                    r"import requests\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported via requests-scala",
                        "HTTP request is sent with requests client",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"import scalaxb\.",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP client is imported via scalaxb",
                        "SOAP web services are interacted with scalaxb",
                    ),
                ),
                (
                    r"import javax\.xml\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP web service is integrated with import",
                        "SOAP service is accessed via JAX-WS",
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
                        "Messaging is enabled by Kafka import",
                        "Message broker is accessed via Kafka",
                    ),
                ),
                (
                    r"KafkaProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Messages are published to Kafka topics",
                        "Kafka message is produced to topic",
                    ),
                ),
                (
                    r"KafkaConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Messages are consumed from Kafka topics",
                        "Kafka message is consumed from topic",
                    ),
                ),
                (
                    r"import com\.rabbitmq",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Messaging is enabled by RabbitMQ import",
                        "Message queue is interacted with RabbitMQ",
                    ),
                ),
                (
                    r"import fs2\.kafka",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kafka streaming is handled with functional library",
                        "Kafka streams are interacted with fs2-kafka",
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
                        "TCP server is created with socket",
                        "TCP socket connections are accepted",
                    ),
                ),
                (
                    r"DatagramSocket\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP communication is enabled by socket creation",
                        "UDP datagrams are interacted with",
                    ),
                ),
                (
                    r"import java\.nio\.channels",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Non-blocking I/O is enabled by channels import",
                        "Network channel is accessed via NIO",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"import slick\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is accessed with functional library",
                        "Relational database is interacted with Slick",
                    ),
                ),
                (
                    r"import doobie\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is accessed with functional library",
                        "Database is queried via Doobie",
                    ),
                ),
                (
                    r"import quill\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database queries are compiled with library import",
                        "Database is queried with Quill",
                    ),
                ),
                (
                    r"import scalikejdbc\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JDBC database is accessed with library import",
                        "Relational database is queried with ScalikeJDBC",
                    ),
                ),
                (
                    r"import reactivemongo\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB is accessed with reactive library",
                        "MongoDB database is queried with ReactiveMongo",
                    ),
                ),
                (
                    r"import neotypes\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j database is accessed with library import",
                        "Neo4j graph database is queried with Neotypes",
                    ),
                ),
                (
                    r"DriverManager\.getConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is opened with credentials",
                        "Database connection is opened via JDBC",
                    ),
                ),
                (
                    r"import anorm\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL database is accessed with library import",
                        "Database is queried via Anorm",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"import scala\.io\.Source",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Files are read with source import",
                        "File source data is received",
                    ),
                ),
                (
                    r"import java\.nio\.file",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File system is accessed with NIO import",
                        "File system is accessed via NIO",
                    ),
                ),
                (
                    r"import better\.files",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File system is accessed with library import",
                        "File system is interacted with better-files",
                    ),
                ),
                (
                    r"import os\.",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File operations are imported via os-lib",
                        "File system is interacted with os-lib",
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
                        "RPC communication is enabled by import",
                        "RPC service is interacted with gRPC",
                    ),
                ),
                (
                    r"import scalapb\.",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Protocol Buffers are supported for gRPC",
                        "gRPC services are interacted with ScalaPB",
                    ),
                ),
                (
                    r"StreamObserver",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "RPC streaming is handled by observer",
                        "Streaming RPC calls are interacted with gRPC",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"import sangria\.",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL server is built with library import",
                        "GraphQL API is interacted with Sangria",
                    ),
                ),
                (
                    r"import caliban\.",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL server is built with library import",
                        "GraphQL API is accessed via Caliban",
                    ),
                ),
                (
                    r"GraphQLSchema",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL schema is defined for server",
                        "GraphQL schema is accessed",
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
                        "Email is sent with JavaMail import",
                        "Email message is sent via JavaMail",
                    ),
                ),
                (
                    r"import jakarta\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent with Jakarta Mail import",
                        "Email message is sent via Jakarta Mail",
                    ),
                ),
                (
                    r"import courier\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent with library import",
                        "Email message is sent via Courier",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"import scalacache\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache is accessed with library import",
                        "Cache layer is written with ScalaCache",
                    ),
                ),
                (
                    r"import redis\.clients\.jedis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is accessed with client import",
                        "Redis cache is written via Jedis",
                    ),
                ),
                (
                    r"import io\.lettuce",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is accessed with async client",
                        "Redis cache is written with Lettuce client",
                    ),
                ),
                (
                    r"import com\.github\.benmanes\.caffeine",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "In-process cache is accessed with library import",
                        "Cache is written via Caffeine",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Event streaming is indicated by content type",
                        "Server-sent event streams are interacted with",
                    ),
                ),
                (
                    r"Source\.tick",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Periodic streaming is created with tick source",
                        "Event stream is processed via Akka Streams",
                    ),
                ),
                (
                    r"ServerSentEvent",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-sent events are handled for streaming",
                        "Server-sent event streams are exposed for inbound",
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
                        "FTP file transfer is handled by client",
                        "FTP server connection is opened via Commons",
                    ),
                ),
                (
                    r"JSch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SSH connectivity is established for file transfer",
                        "SFTP server connection is opened via JSch",
                    ),
                ),
                (
                    r"ChannelSftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP operations are performed on files",
                        "SFTP server is written via JSch",
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
                        "Jobs are scheduled with Quartz import",
                        "Scheduled jobs are interacted with Quartz",
                    ),
                ),
                (
                    r"import akka\.scheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Actor scheduling is enabled by import",
                        "Actor message is scheduled via Akka",
                    ),
                ),
                (
                    r"system\.scheduler",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduled tasks are executed by system scheduler",
                        "Actor scheduler is accessed via Akka",
                    ),
                ),
            ],
        },
    },
)
