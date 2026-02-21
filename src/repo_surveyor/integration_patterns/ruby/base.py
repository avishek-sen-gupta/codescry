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
                        "Ruby Net::HTTP library making outbound HTTP requests",
                        "This code uses Ruby Net::HTTP to send outbound HTTP requests",
                    ),
                ),
                (
                    r"require ['\"]net/http['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby net/http library import for HTTP client usage",
                        "This code uses Ruby net/http to send outbound HTTP requests",
                    ),
                ),
                (
                    r"HTTParty",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby HTTParty library making outbound HTTP requests",
                        "This code uses Ruby HTTParty to send outbound HTTP requests",
                    ),
                ),
                (
                    r"Faraday\.new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Faraday HTTP client being instantiated",
                        "This code uses Ruby Faraday to send outbound HTTP requests",
                    ),
                ),
                (
                    r"RestClient\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby RestClient library making outbound HTTP requests",
                        "This code uses Ruby RestClient to send outbound HTTP requests",
                    ),
                ),
                (
                    r"require ['\"]typhoeus['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Typhoeus HTTP client library import",
                        "This code uses Ruby Typhoeus to send outbound HTTP requests",
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
                        "Ruby Savon SOAP client library import",
                        "This code uses Ruby Savon to send outbound SOAP requests",
                    ),
                ),
                (
                    r"Savon\.client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Savon SOAP client being instantiated",
                        "This code uses Ruby Savon to connect to an outbound SOAP service",
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
                        "Ruby Bunny AMQP client library import",
                        "This code uses Ruby Bunny to interact with an AMQP message broker",
                    ),
                ),
                (
                    r"Bunny\.new",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby Bunny AMQP connection being instantiated",
                        "This code uses Ruby Bunny to interact with an AMQP message broker",
                    ),
                ),
                (
                    r"require ['\"]ruby-kafka['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby ruby-kafka Kafka client library import",
                        "This code uses Ruby ruby-kafka to interact with a Kafka cluster",
                    ),
                ),
                (
                    r"Kafka::Consumer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ruby Kafka::Consumer consuming messages from a Kafka topic",
                        "This code uses Ruby Kafka to receive inbound messages from a Kafka topic",
                    ),
                ),
                (
                    r"Kafka::Producer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Kafka::Producer producing messages to a Kafka topic",
                        "This code uses Ruby Kafka to produce outbound messages to a Kafka topic",
                    ),
                ),
                (
                    r"require ['\"]sneakers['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ruby Sneakers background worker library import",
                        "This code uses Ruby Sneakers to listen for inbound AMQP messages",
                    ),
                ),
                (
                    r"Sidekiq",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby Sidekiq background job framework usage",
                        "This code uses Ruby Sidekiq to interact with a background job queue",
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
                        "Ruby TCPServer accepting inbound TCP connections",
                        "This code uses Ruby TCPServer to accept inbound TCP connections",
                    ),
                ),
                (
                    r"TCPSocket\.new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby TCPSocket connecting to an outbound TCP endpoint",
                        "This code uses Ruby TCPSocket to connect to an outbound TCP endpoint",
                    ),
                ),
                (
                    r"UDPSocket\.new",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby UDPSocket for UDP communication",
                        "This code uses Ruby UDPSocket to interact with a UDP endpoint",
                    ),
                ),
                (
                    r"UNIXSocket",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby UNIXSocket for local IPC communication",
                        "This code uses Ruby UNIXSocket to interact with a Unix domain socket",
                    ),
                ),
                (
                    r"require ['\"]faye-websocket['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby faye-websocket WebSocket library import",
                        "This code uses Ruby faye-websocket to interact with a WebSocket connection",
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
                        "Ruby ActiveRecord::Base ORM base class for database models",
                        "This code uses Ruby ActiveRecord to query an outbound relational database",
                    ),
                ),
                (
                    r"ApplicationRecord",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ApplicationRecord ORM model base class",
                        "This code uses Ruby ActiveRecord to query an outbound relational database",
                    ),
                ),
                (
                    r"ActiveRecord::Migration",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord::Migration defining a database schema migration",
                        "This code uses Ruby ActiveRecord to write to an outbound database schema",
                    ),
                ),
                (
                    r"\bhas_many\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord has_many association declaration",
                        "This code uses Ruby ActiveRecord to query outbound has-many database relationships",
                    ),
                ),
                (
                    r"\bhas_one\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord has_one association declaration",
                        "This code uses Ruby ActiveRecord to query outbound has-one database relationships",
                    ),
                ),
                (
                    r"\bbelongs_to\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord belongs_to association declaration",
                        "This code uses Ruby ActiveRecord to query outbound belongs-to database relationships",
                    ),
                ),
                (
                    r"\bhas_and_belongs_to_many\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord has_and_belongs_to_many association declaration",
                        "This code uses Ruby ActiveRecord to query outbound many-to-many database relationships",
                    ),
                ),
                (
                    r"\bcreate_table\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord create_table migration command",
                        "This code uses Ruby ActiveRecord to write a new table to an outbound database",
                    ),
                ),
                (
                    r"\badd_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord add_column migration command",
                        "This code uses Ruby ActiveRecord to write a new column to an outbound database",
                    ),
                ),
                (
                    r"\badd_index\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord add_index migration command",
                        "This code uses Ruby ActiveRecord to write a new index to an outbound database",
                    ),
                ),
                (
                    r"\badd_reference\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord add_reference migration command",
                        "This code uses Ruby ActiveRecord to write a foreign key reference to an outbound database",
                    ),
                ),
                (
                    r"\bremove_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord remove_column migration command",
                        "This code uses Ruby ActiveRecord to remove a column from an outbound database",
                    ),
                ),
                (
                    r"\brename_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord rename_column migration command",
                        "This code uses Ruby ActiveRecord to rename a column in an outbound database",
                    ),
                ),
                (
                    r"\bchange_column\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord change_column migration command",
                        "This code uses Ruby ActiveRecord to alter a column in an outbound database",
                    ),
                ),
                (
                    r"\.where\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord where clause querying the database",
                        "This code uses Ruby ActiveRecord to query an outbound database with conditions",
                    ),
                ),
                (
                    r"\.find_by\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord find_by querying the database by attribute",
                        "This code uses Ruby ActiveRecord to query an outbound database by attribute",
                    ),
                ),
                (
                    r"\.joins\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord joins clause for SQL join queries",
                        "This code uses Ruby ActiveRecord to query an outbound database with joins",
                    ),
                ),
                (
                    r"\.includes\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord includes for eager loading associations",
                        "This code uses Ruby ActiveRecord to query outbound database associations eagerly",
                    ),
                ),
                (
                    r"\.eager_load\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord eager_load for SQL LEFT OUTER JOIN loading",
                        "This code uses Ruby ActiveRecord to eagerly load outbound database associations",
                    ),
                ),
                (
                    r"\.preload\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord preload for separate query association loading",
                        "This code uses Ruby ActiveRecord to preload outbound database associations",
                    ),
                ),
                (
                    r"\bscope\b\s+:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord scope defining a reusable query",
                        "This code uses Ruby ActiveRecord to define outbound database query scopes",
                    ),
                ),
                (
                    r"\bvalidates\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord validates declaring model validation rules",
                        "This code uses Ruby ActiveRecord to validate data before writing to an outbound database",
                    ),
                ),
                (
                    r"\bvalidate\b\s+:",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveRecord validate declaring a custom validation method",
                        "This code uses Ruby ActiveRecord to apply custom validation before writing to an outbound database",
                    ),
                ),
                (
                    r"require ['\"]sequel['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Sequel ORM library import",
                        "This code uses Ruby Sequel to connect to an outbound database",
                    ),
                ),
                (
                    r"Sequel\.connect",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Sequel database connection being established",
                        "This code uses Ruby Sequel to connect to an outbound database",
                    ),
                ),
                (
                    r"require ['\"]pg['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby pg PostgreSQL client library import",
                        "This code uses Ruby pg to connect to an outbound PostgreSQL database",
                    ),
                ),
                (
                    r"PG::Connection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby PG::Connection establishing a PostgreSQL connection",
                        "This code uses Ruby PG to connect to an outbound PostgreSQL database",
                    ),
                ),
                (
                    r"require ['\"]mysql2['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby mysql2 MySQL client library import",
                        "This code uses Ruby mysql2 to connect to an outbound MySQL database",
                    ),
                ),
                (
                    r"Mysql2::Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Mysql2::Client establishing a MySQL connection",
                        "This code uses Ruby Mysql2 to connect to an outbound MySQL database",
                    ),
                ),
                (
                    r"require ['\"]mongo['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby mongo MongoDB client library import",
                        "This code uses Ruby mongo to connect to an outbound MongoDB database",
                    ),
                ),
                (
                    r"Mongo::Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Mongo::Client establishing a MongoDB connection",
                        "This code uses Ruby Mongo to connect to an outbound MongoDB database",
                    ),
                ),
                (
                    r"Mongoid::Document",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Mongoid::Document ODM document model declaration",
                        "This code uses Ruby Mongoid to interact with an outbound MongoDB database",
                    ),
                ),
                (
                    r"require ['\"]neo4j['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby neo4j graph database client library import",
                        "This code uses Ruby neo4j to connect to an outbound Neo4j database",
                    ),
                ),
                (
                    r"Neo4j::Driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Neo4j::Driver establishing a Neo4j database connection",
                        "This code uses Ruby Neo4j to connect to an outbound Neo4j database",
                    ),
                ),
                (
                    r"require ['\"]activegraph['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby activegraph graph ORM library import",
                        "This code uses Ruby ActiveGraph to interact with an outbound graph database",
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
                        "Ruby File.open opening a file for I/O",
                        "This code uses Ruby File to write to or read from a local file",
                    ),
                ),
                (
                    r"File\.read\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby File.read reading file contents",
                        "This code uses Ruby File to read from a local file",
                    ),
                ),
                (
                    r"File\.write\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby File.write writing data to a file",
                        "This code uses Ruby File to write to a local file",
                    ),
                ),
                (
                    r"IO\.read\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby IO.read reading data from an IO stream",
                        "This code uses Ruby IO to read from a file or stream",
                    ),
                ),
                (
                    r"Aws::S3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Aws::S3 client interacting with AWS S3 storage",
                        "This code uses Ruby AWS SDK to write to or read from outbound S3 object storage",
                    ),
                ),
                (
                    r"require ['\"]fog['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby fog cloud storage library import",
                        "This code uses Ruby Fog to interact with outbound cloud storage",
                    ),
                ),
                (
                    r"CarrierWave",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby CarrierWave file upload library usage",
                        "This code uses Ruby CarrierWave to write uploaded files to outbound storage",
                    ),
                ),
                (
                    r"ActiveStorage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ActiveStorage file attachment framework usage",
                        "This code uses Ruby ActiveStorage to write to or read from outbound file storage",
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
                        "Ruby grpc library import for gRPC communication",
                        "This code uses Ruby gRPC to interact with a gRPC service",
                    ),
                ),
                (
                    r"GRPC::RpcServer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ruby GRPC::RpcServer accepting inbound gRPC calls",
                        "This code uses Ruby gRPC to handle inbound gRPC requests",
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
                        "Ruby GraphQL::Schema defining an inbound GraphQL API schema",
                        "This code uses Ruby GraphQL to expose an inbound GraphQL API schema",
                    ),
                ),
                (
                    r"require ['\"]graphql['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ruby graphql library import for GraphQL API definition",
                        "This code uses Ruby GraphQL to expose an inbound GraphQL API",
                    ),
                ),
                (
                    r"GraphQL::ObjectType",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ruby GraphQL::ObjectType defining a GraphQL object type",
                        "This code uses Ruby GraphQL to expose an inbound GraphQL object type",
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
                        "Ruby ActionMailer framework sending outbound email",
                        "This code uses Ruby ActionMailer to send outbound email messages",
                    ),
                ),
                (
                    r"< ApplicationMailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby ApplicationMailer subclass for sending email",
                        "This code uses Ruby ActionMailer to send outbound email messages",
                    ),
                ),
                (
                    r"Mail\.deliver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Mail.deliver sending an outbound email message",
                        "This code uses Ruby Mail to send an outbound email message",
                    ),
                ),
                (
                    r"require ['\"]mail['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby mail library import for email handling",
                        "This code uses Ruby mail to send outbound email messages",
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
                        "Ruby Rails.cache accessing the Rails cache store",
                        "This code uses Ruby Rails to write to or read from an outbound cache store",
                    ),
                ),
                (
                    r"require ['\"]redis['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby redis client library import",
                        "This code uses Ruby Redis to connect to an outbound Redis cache",
                    ),
                ),
                (
                    r"Redis\.new",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Redis client being instantiated",
                        "This code uses Ruby Redis to connect to an outbound Redis cache",
                    ),
                ),
                (
                    r"require ['\"]dalli['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Dalli Memcached client library import",
                        "This code uses Ruby Dalli to connect to an outbound Memcached cache",
                    ),
                ),
                (
                    r"Dalli::Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Dalli::Client establishing a Memcached connection",
                        "This code uses Ruby Dalli to connect to an outbound Memcached cache",
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
                        "Ruby ActionController::Live enabling streaming responses to clients",
                        "This code uses Ruby ActionController to handle inbound streaming connections",
                    ),
                ),
                (
                    r"SSE\.new",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Ruby SSE.new creating a Server-Sent Events stream for clients",
                        "This code uses Ruby SSE to handle inbound Server-Sent Events connections",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby text/event-stream content type for SSE communication",
                        "This code uses Ruby to interact with a Server-Sent Events stream",
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
                        "Ruby net/ftp library import for FTP client usage",
                        "This code uses Ruby net/ftp to connect to an outbound FTP server",
                    ),
                ),
                (
                    r"require ['\"]net/sftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby net/sftp library import for SFTP client usage",
                        "This code uses Ruby net/sftp to connect to an outbound SFTP server",
                    ),
                ),
                (
                    r"Net::FTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Net::FTP client connecting to an FTP server",
                        "This code uses Ruby Net::FTP to connect to an outbound FTP server",
                    ),
                ),
                (
                    r"Net::SFTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Ruby Net::SFTP client connecting to an SFTP server",
                        "This code uses Ruby Net::SFTP to connect to an outbound SFTP server",
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
                        "Ruby Whenever cron scheduling library import",
                        "This code uses Ruby Whenever to interact with scheduled cron tasks",
                    ),
                ),
                (
                    r"require ['\"]rufus-scheduler['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby Rufus-Scheduler in-process job scheduler library import",
                        "This code uses Ruby Rufus-Scheduler to interact with scheduled jobs",
                    ),
                ),
                (
                    r"require ['\"]clockwork['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby Clockwork process-based scheduler library import",
                        "This code uses Ruby Clockwork to interact with scheduled recurring tasks",
                    ),
                ),
                (
                    r"Rufus::Scheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Ruby Rufus::Scheduler being instantiated for job scheduling",
                        "This code uses Ruby Rufus-Scheduler to interact with scheduled jobs",
                    ),
                ),
            ],
        },
    },
)
