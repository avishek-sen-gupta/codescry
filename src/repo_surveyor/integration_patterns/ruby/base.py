"""Ruby base integration patterns."""

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
                    r"Net::HTTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP requests are made by Net library",
                        "HTTP request is sent to endpoint",
                    ),
                ),
                (
                    r"require ['\"]net/http['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported by net library",
                        "HTTP request is sent to endpoint",
                    ),
                ),
                (
                    r"HTTParty",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP requests are made by HTTParty library",
                        "HTTP request is sent outbound",
                    ),
                ),
                (
                    r"Faraday\.new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is created by Faraday library",
                        "HTTP request is sent outbound",
                    ),
                ),
                (
                    r"RestClient\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP requests are made by RestClient library",
                        "HTTP request is sent via REST client",
                    ),
                ),
                (
                    r"require ['\"]typhoeus['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported by Typhoeus library",
                        "HTTP request is sent via client",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]savon['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP client is imported by Savon library",
                        "SOAP request is sent to service",
                    ),
                ),
                (
                    r"Savon\.client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP client is created by Savon library",
                        "SOAP service connection is opened",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]bunny['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP client is imported by Bunny library",
                        "AMQP message broker is accessed",
                    ),
                ),
                (
                    r"Bunny\.new",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP connection is created by Bunny client",
                        "AMQP message broker is accessed",
                    ),
                ),
                (
                    r"require ['\"]ruby-kafka['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kafka client is imported",
                        "Kafka cluster is accessed for messaging",
                    ),
                ),
                (
                    r"Kafka::Consumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Messages are consumed from Kafka topic",
                        "Kafka message is received from topic",
                    ),
                ),
                (
                    r"Kafka::Producer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Messages are produced to Kafka topic",
                        "Kafka message is produced to topic",
                    ),
                ),
                (
                    r"require ['\"]sneakers['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Background worker is imported by Sneakers library",
                        "AMQP message is received from queue",
                    ),
                ),
                (
                    r"Sidekiq",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background jobs are handled by Sidekiq framework",
                        "Background job is queued for processing",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"TCPServer\.new",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TCP connections are accepted by TCP server",
                        "TCP connection is accepted for inbound requests",
                    ),
                ),
                (
                    r"TCPSocket\.new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TCP endpoint is connected by socket",
                        "TCP connection is opened to endpoint",
                    ),
                ),
                (
                    r"UDPSocket\.new",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "UDP communication is handled by socket",
                        "UDP socket is opened for communication",
                    ),
                ),
                (
                    r"UNIXSocket",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "IPC communication is handled by UNIX socket",
                        "Unix socket is opened for communication",
                    ),
                ),
                (
                    r"require ['\"]faye-websocket['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WebSocket is imported by faye library",
                        "WebSocket connection is opened for bidirectional communication",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"ActiveRecord::Base",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database models inherit from ORM base",
                        "Relational database is queried",
                    ),
                ),
                (
                    r"ApplicationRecord",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM model is created by ApplicationRecord base",
                        "Relational database is queried",
                    ),
                ),
                (
                    r"ActiveRecord::Migration",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database schema migration is defined",
                        "Database schema is modified",
                    ),
                ),
                (
                    r"\bhas_many\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "One-to-many association is declared by has_many",
                        "Has-many relationship is queried",
                    ),
                ),
                (
                    r"\bhas_one\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "One-to-one association is declared by has_one",
                        "Has-one relationship is queried",
                    ),
                ),
                (
                    r"\bbelongs_to\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Association is declared by belongs_to",
                        "Belongs-to relationship is queried",
                    ),
                ),
                (
                    r"\bhas_and_belongs_to_many\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Many-to-many association is declared",
                        "Many-to-many relationship is queried",
                    ),
                ),
                (
                    r"\bcreate_table\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is created by migration",
                        "Database table is created",
                    ),
                ),
                (
                    r"\badd_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is added by migration",
                        "Database column is created",
                    ),
                ),
                (
                    r"\badd_index\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database index is added by migration",
                        "Database index is created",
                    ),
                ),
                (
                    r"\badd_reference\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database reference is added by migration",
                        "Foreign key reference is written to database",
                    ),
                ),
                (
                    r"\bremove_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is removed by migration",
                        "Database column is removed",
                    ),
                ),
                (
                    r"\brename_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is renamed by migration",
                        "Database column is renamed",
                    ),
                ),
                (
                    r"\bchange_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is changed by migration",
                        "Database column is altered",
                    ),
                ),
                (
                    r"\.where\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database is queried by where clause",
                        "Database is queried with conditions",
                    ),
                ),
                (
                    r"\.find_by\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database is queried by attribute finder",
                        "Database is queried by attribute",
                    ),
                ),
                (
                    r"\.joins\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "SQL join queries are performed by joins",
                        "Database is queried with joins",
                    ),
                ),
                (
                    r"\.includes\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Associations are eager loaded by includes",
                        "Database associations are queried eagerly",
                    ),
                ),
                (
                    r"\.eager_load\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "SQL LEFT OUTER JOIN loading is performed",
                        "Database associations are eagerly loaded",
                    ),
                ),
                (
                    r"\.preload\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Associations are loaded by separate queries",
                        "Database associations are preloaded",
                    ),
                ),
                (
                    r"\bscope\b\s+:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Reusable query is defined by scope",
                        "Database query scopes are defined",
                    ),
                ),
                (
                    r"\bvalidates\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Model validation rules are declared",
                        "Data validation is applied before database write",
                    ),
                ),
                (
                    r"\bvalidate\b\s+:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Custom validation method is declared",
                        "Custom validation is applied before database write",
                    ),
                ),
                (
                    r"require ['\"]sequel['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ORM library is imported by Sequel",
                        "Database connection is opened via ORM",
                    ),
                ),
                (
                    r"Sequel\.connect",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database connection is established by Sequel",
                        "Database connection is opened via ORM",
                    ),
                ),
                (
                    r"require ['\"]pg['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL client is imported",
                        "PostgreSQL connection is opened for database access",
                    ),
                ),
                (
                    r"PG::Connection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL connection is established by PG client",
                        "PostgreSQL connection is opened for database access",
                    ),
                ),
                (
                    r"require ['\"]mysql2['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL client is imported by mysql2 library",
                        "MySQL connection is opened for database access",
                    ),
                ),
                (
                    r"Mysql2::Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL connection is established by Mysql2 client",
                        "MySQL connection is opened for database access",
                    ),
                ),
                (
                    r"require ['\"]mongo['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB client is imported by mongo library",
                        "MongoDB connection is opened for database access",
                    ),
                ),
                (
                    r"Mongo::Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB connection is established by Mongo client",
                        "MongoDB connection is opened for database access",
                    ),
                ),
                (
                    r"Mongoid::Document",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ODM document model is declared by Mongoid",
                        "MongoDB database is accessed via ORM",
                    ),
                ),
                (
                    r"require ['\"]neo4j['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database client is imported by neo4j library",
                        "Neo4j connection is opened for graph database",
                    ),
                ),
                (
                    r"Neo4j::Driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j connection is established by database driver",
                        "Neo4j connection is opened for graph database",
                    ),
                ),
                (
                    r"require ['\"]activegraph['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph ORM is imported by activegraph library",
                        "Graph database is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"File\.open\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "File is opened for I/O operations",
                        "Local file is accessed for read-write operations",
                    ),
                ),
                (
                    r"File\.read\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "File contents is read from filesystem",
                        "Local file is read",
                    ),
                ),
                (
                    r"File\.write\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Data is written to file",
                        "Local file is written",
                    ),
                ),
                (
                    r"IO\.read\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Data is read from IO stream",
                        "File stream is read",
                    ),
                ),
                (
                    r"Aws::S3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 storage is accessed by AWS client",
                        "S3 object storage is accessed for read-write operations",
                    ),
                ),
                (
                    r"require ['\"]fog['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud storage is imported by fog library",
                        "Cloud storage is accessed",
                    ),
                ),
                (
                    r"CarrierWave",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File upload is handled by CarrierWave library",
                        "Uploaded files are written to storage",
                    ),
                ),
                (
                    r"ActiveStorage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File attachment is handled by ActiveStorage framework",
                        "File storage is accessed for read-write operations",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]grpc['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "gRPC communication is imported by library",
                        "gRPC service is accessed via client",
                    ),
                ),
                (
                    r"GRPC::RpcServer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC calls are accepted by RPC server",
                        "gRPC request is handled for inbound calls",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"GraphQL::Schema",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL API schema is defined for inbound requests",
                        "GraphQL schema is exposed for inbound requests",
                    ),
                ),
                (
                    r"require ['\"]graphql['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL API is imported for definition",
                        "GraphQL API is exposed for inbound requests",
                    ),
                ),
                (
                    r"GraphQL::ObjectType",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL object type is defined",
                        "GraphQL object type is exposed",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"ActionMailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent by ActionMailer framework",
                        "Email message is sent via outbound service",
                    ),
                ),
                (
                    r"< ApplicationMailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent by ApplicationMailer subclass",
                        "Email message is sent via outbound service",
                    ),
                ),
                (
                    r"Mail\.deliver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is delivered by Mail",
                        "Email message is sent via SMTP",
                    ),
                ),
                (
                    r"require ['\"]mail['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email handling is imported by mail library",
                        "Email message is sent via SMTP",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"Rails\.cache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache store is accessed by Rails",
                        "Cache store is accessed for read/write operations",
                    ),
                ),
                (
                    r"require ['\"]redis['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis client is imported",
                        "Redis connection is opened for cache access",
                    ),
                ),
                (
                    r"Redis\.new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis client is created",
                        "Redis connection is opened for cache access",
                    ),
                ),
                (
                    r"require ['\"]dalli['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached client is imported by Dalli library",
                        "Memcached cache is connected",
                    ),
                ),
                (
                    r"Dalli::Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached connection is established by Dalli client",
                        "Memcached cache is connected",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"ActionController::Live",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Streaming responses are enabled to clients",
                        "Streaming connection is handled for inbound requests",
                    ),
                ),
                (
                    r"SSE\.new",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-Sent Events stream is created for clients",
                        "Server-Sent Events connection is handled for inbound streams",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SSE stream is configured with content type",
                        "Server-Sent Events stream is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]net/ftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP client is imported by net library",
                        "FTP connection is opened to server",
                    ),
                ),
                (
                    r"require ['\"]net/sftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP client is imported by net library",
                        "SFTP connection is opened to server",
                    ),
                ),
                (
                    r"Net::FTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP server is connected by Net client",
                        "FTP connection is opened to server",
                    ),
                ),
                (
                    r"Net::SFTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP server is connected by Net client",
                        "SFTP connection is opened to server",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"require ['\"]whenever['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cron scheduling is imported by Whenever library",
                        "Cron tasks are scheduled via job manager",
                    ),
                ),
                (
                    r"require ['\"]rufus-scheduler['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job scheduler is imported by Rufus library",
                        "Scheduled jobs are managed by scheduler",
                    ),
                ),
                (
                    r"require ['\"]clockwork['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Process scheduler is imported by Clockwork library",
                        "Scheduled recurring tasks are configured",
                    ),
                ),
                (
                    r"Rufus::Scheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job scheduler is created by Rufus library",
                        "Scheduled jobs are managed by scheduler",
                    ),
                ),
            ],
        },
    },
)
