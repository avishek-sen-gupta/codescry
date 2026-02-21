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
                        "Scala sttp client import for HTTP client requests",
                        "This code uses Scala sttp to send outbound HTTP requests",
                    ),
                ),
                (
                    r"import scalaj\.http",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala scalaj-http import for HTTP client requests",
                        "This code uses Scala scalaj to send outbound HTTP requests",
                    ),
                ),
                (
                    r"import dispatch\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Dispatch library import for async HTTP client requests",
                        "This code uses Scala Dispatch to send outbound HTTP requests",
                    ),
                ),
                (
                    r"HttpURLConnection",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Scala HttpURLConnection usage for HTTP client",
                        "This code uses Scala to send outbound HTTP requests",
                    ),
                ),
                (
                    r"import requests\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala requests-scala import for HTTP client requests",
                        "This code uses Scala requests to send outbound HTTP requests",
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
                        "Scala scalaxb import for SOAP/XML web services",
                        "This code uses Scala scalaxb to interact with SOAP web services",
                    ),
                ),
                (
                    r"import javax\.xml\.ws",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala JAX-WS import for SOAP web service integration",
                        "This code uses Scala JAX-WS to interact with SOAP web services",
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
                        "Scala Apache Kafka import for messaging",
                        "This code uses Scala Apache Kafka to interact with a message broker",
                    ),
                ),
                (
                    r"KafkaProducer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala KafkaProducer for message publishing",
                        "This code uses Scala Kafka to produce outgoing messages to a topic",
                    ),
                ),
                (
                    r"KafkaConsumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Scala KafkaConsumer for message consumption",
                        "This code uses Scala Kafka to receive inbound messages from a topic",
                    ),
                ),
                (
                    r"import com\.rabbitmq",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala RabbitMQ import for messaging",
                        "This code uses Scala RabbitMQ to interact with a message queue",
                    ),
                ),
                (
                    r"import fs2\.kafka",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala fs2-kafka import for functional Kafka streaming",
                        "This code uses Scala fs2-kafka to interact with Kafka streams",
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
                        "Scala ServerSocket instantiation for TCP server",
                        "This code uses Scala to accept inbound TCP socket connections",
                    ),
                ),
                (
                    r"DatagramSocket\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala DatagramSocket instantiation for UDP communication",
                        "This code uses Scala to interact with UDP datagrams",
                    ),
                ),
                (
                    r"import java\.nio\.channels",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala Java NIO channels import for non-blocking I/O",
                        "This code uses Scala Java NIO to interact with network channels",
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
                        "Scala Slick library import for database access",
                        "This code uses Scala Slick to interact with a relational database",
                    ),
                ),
                (
                    r"import doobie\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Doobie library import for functional database access",
                        "This code uses Scala Doobie to query a relational database",
                    ),
                ),
                (
                    r"import quill\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Quill library import for compile-time database queries",
                        "This code uses Scala Quill to query a database",
                    ),
                ),
                (
                    r"import scalikejdbc\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala ScalikeJDBC library import for JDBC database access",
                        "This code uses Scala ScalikeJDBC to query a relational database",
                    ),
                ),
                (
                    r"import reactivemongo\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala ReactiveMongo library import for MongoDB access",
                        "This code uses Scala ReactiveMongo to query a MongoDB database",
                    ),
                ),
                (
                    r"import neotypes\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Neotypes library import for Neo4j graph database access",
                        "This code uses Scala Neotypes to query a Neo4j graph database",
                    ),
                ),
                (
                    r"DriverManager\.getConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala JDBC DriverManager.getConnection for database connectivity",
                        "This code uses Scala JDBC to connect to a relational database",
                    ),
                ),
                (
                    r"import anorm\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Anorm library import for SQL database access",
                        "This code uses Scala Anorm to query a relational database",
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
                        "Scala io.Source import for file reading",
                        "This code uses Scala to receive data from a file source",
                    ),
                ),
                (
                    r"import java\.nio\.file",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala Java NIO file import for file system access",
                        "This code uses Scala Java NIO to interact with the file system",
                    ),
                ),
                (
                    r"import better\.files",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala better-files library import for file system access",
                        "This code uses Scala better-files to interact with the file system",
                    ),
                ),
                (
                    r"import os\.",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala os-lib import for operating system file operations",
                        "This code uses Scala os-lib to interact with the file system",
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
                        "Scala gRPC import for RPC communication",
                        "This code uses Scala gRPC to interact with a remote procedure call service",
                    ),
                ),
                (
                    r"import scalapb\.",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala ScalaPB import for Protocol Buffer and gRPC support",
                        "This code uses Scala ScalaPB to interact with gRPC services",
                    ),
                ),
                (
                    r"StreamObserver",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala gRPC StreamObserver for streaming RPC",
                        "This code uses Scala gRPC to interact with streaming RPC calls",
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
                        "Scala Sangria library import for GraphQL server",
                        "This code uses Scala Sangria to interact with a GraphQL API",
                    ),
                ),
                (
                    r"import caliban\.",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala Caliban library import for GraphQL server/client",
                        "This code uses Scala Caliban to interact with a GraphQL API",
                    ),
                ),
                (
                    r"GraphQLSchema",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala GraphQL GraphQLSchema for schema definition",
                        "This code uses Scala GraphQL to interact with a GraphQL schema",
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
                        "Scala JavaMail import for email sending",
                        "This code uses Scala JavaMail to send outbound email messages",
                    ),
                ),
                (
                    r"import jakarta\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Jakarta Mail import for email sending",
                        "This code uses Scala Jakarta Mail to send outbound email messages",
                    ),
                ),
                (
                    r"import courier\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Courier library import for email sending",
                        "This code uses Scala Courier to send outbound email messages",
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
                        "Scala ScalaCache library import for cache access",
                        "This code uses Scala ScalaCache to write to a cache layer",
                    ),
                ),
                (
                    r"import redis\.clients\.jedis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Jedis import for Redis cache access",
                        "This code uses Scala Jedis to write to a Redis cache",
                    ),
                ),
                (
                    r"import io\.lettuce",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Lettuce import for Redis cache access",
                        "This code uses Scala Lettuce to write to a Redis cache",
                    ),
                ),
                (
                    r"import com\.github\.benmanes\.caffeine",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala Caffeine library import for in-process cache",
                        "This code uses Scala Caffeine to write to an in-process cache",
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
                        "Scala SSE content type for event streaming",
                        "This code uses Scala to interact with server-sent event streams",
                    ),
                ),
                (
                    r"Source\.tick",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala Akka Source.tick for periodic streaming",
                        "This code uses Scala Akka Streams to interact with periodic event streams",
                    ),
                ),
                (
                    r"ServerSentEvent",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Scala ServerSentEvent for server-sent event handling",
                        "This code uses Scala to expose inbound server-sent event streams",
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
                        "Scala Apache Commons FTPClient for FTP file transfer",
                        "This code uses Scala Apache Commons to connect to an FTP server",
                    ),
                ),
                (
                    r"JSch\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala JSch instantiation for SSH/SFTP connectivity",
                        "This code uses Scala JSch to connect to an SFTP server",
                    ),
                ),
                (
                    r"ChannelSftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Scala JSch ChannelSftp for SFTP file operations",
                        "This code uses Scala JSch to write to an SFTP server",
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
                        "Scala Quartz scheduler import for job scheduling",
                        "This code uses Scala Quartz to interact with scheduled jobs",
                    ),
                ),
                (
                    r"import akka\.scheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala Akka scheduler import for actor-based scheduling",
                        "This code uses Scala Akka to interact with scheduled actor messages",
                    ),
                ),
                (
                    r"system\.scheduler",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scala Akka system.scheduler for scheduled task execution",
                        "This code uses Scala Akka to interact with the actor system scheduler",
                    ),
                ),
            ],
        },
    },
)
