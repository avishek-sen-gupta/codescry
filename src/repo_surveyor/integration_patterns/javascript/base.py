"""JavaScript base integration patterns."""

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
                    r"require\(['\"]fastify['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Fastify",
                        "HTTP server is handled for inbound requests",
                    ),
                ),
                (
                    r"require\(['\"]koa['\"]\)",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Koa",
                        "HTTP server is handled for inbound requests",
                    ),
                ),
                (
                    r"require\(['\"]axios['\"]\)",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported for outbound requests",
                        "HTTP request is sent via Axios",
                    ),
                ),
                (
                    r"fetch\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP requests are made with fetch API",
                        "HTTP requests are sent via fetch",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]soap['\"]\)",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP web service is integrated",
                        "SOAP service is accessed for web communication",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]kafkajs['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kafka messaging is imported for stream processing",
                        "Kafka broker is accessed via KafkaJS",
                    ),
                ),
                (
                    r"require\(['\"]amqplib['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP messaging is imported for RabbitMQ",
                        "AMQP broker is accessed via amqplib",
                    ),
                ),
                (
                    r"require\(['\"]bull['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job queue is imported with Redis backend",
                        "Job queue is accessed via Bull",
                    ),
                ),
                (
                    r"consumer\.run\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kafka messages are consumed by consumer",
                        "Kafka message is received for inbound",
                    ),
                ),
                (
                    r"producer\.send\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kafka messages are published by producer",
                        "Kafka message is produced for outbound",
                    ),
                ),
                (
                    r"require\(['\"]@aws-sdk/client-sqs['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SQS client is imported for message queue",
                        "SQS queue is accessed via SDK",
                    ),
                ),
                (
                    r"require\(['\"]@aws-sdk/client-sns['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SNS client is imported for notification service",
                        "SNS notifications are accessed via SDK",
                    ),
                ),
                (
                    r"require\(['\"]@google-cloud/pubsub['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pub/Sub messaging is imported for GCP",
                        "Pub/Sub messaging is accessed via Cloud SDK",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]socket.io['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket server is imported via Socket.IO",
                        "WebSocket connection is handled for inbound",
                    ),
                ),
                (
                    r"require\(['\"]ws['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WebSocket communication is imported",
                        "WebSocket connections are handled with message exchange",
                    ),
                ),
                (
                    r"io\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket event listener is registered for connections",
                        "Socket event is listened for inbound",
                    ),
                ),
                (
                    r"socket\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket event listener is registered",
                        "WebSocket events are listened for inbound connections",
                    ),
                ),
                (
                    r"new WebSocket\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "WebSocket client is created for outbound connection",
                        "WebSocket server is connected for outbound communication",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]sequelize['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL database is accessed via Sequelize ORM",
                        "Database is queried via Sequelize",
                    ),
                ),
                (
                    r"require\(['\"]mongoose['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB ODM is imported for data modeling",
                        "MongoDB database is accessed via Mongoose",
                    ),
                ),
                (
                    r"require\(['\"]knex['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL database is accessed via query builder",
                        "Database is queried via Knex",
                    ),
                ),
                (
                    r"require\(['\"]mongodb['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB database is accessed by driver",
                        "MongoDB database is queried via driver",
                    ),
                ),
                (
                    r"mongoose\.Schema",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB schema is defined for data structure",
                        "MongoDB schema is queried via database",
                    ),
                ),
                (
                    r"mongoose\.model\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB model is registered for collection",
                        "MongoDB collection is queried via model",
                    ),
                ),
                (
                    r"Sequelize\.define\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL model is defined by Sequelize",
                        "Database model is queried via Sequelize",
                    ),
                ),
                (
                    r"require\(['\"]@aws-sdk/client-dynamodb['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "DynamoDB client is imported for database access",
                        "DynamoDB database is queried via SDK",
                    ),
                ),
                (
                    r"require\(['\"]firebase-admin['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Firebase database is imported via Admin SDK",
                        "Firebase database is queried via Admin SDK",
                    ),
                ),
                (
                    r"require\(['\"]neo4j-driver['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is accessed via Neo4j driver",
                        "Graph database is queried via Neo4j driver",
                    ),
                ),
                (
                    r"neo4j\.driver\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database connection is created for Neo4j",
                        "Graph database is connected via Neo4j",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]fs['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File system is accessed by fs module",
                        "File system is accessed via fs",
                    ),
                ),
                (
                    r"fs\.readFileSync",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File is read synchronously from system",
                        "File system is read via fs",
                    ),
                ),
                (
                    r"require\(['\"]aws-sdk['\"]\)",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "AWS SDK is imported for cloud service access",
                        "AWS services are accessed via SDK",
                    ),
                ),
                (
                    r"require\(['\"]multer['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "File uploads are handled by Multer middleware",
                        "File upload is accepted for inbound",
                    ),
                ),
                (
                    r"require\(['\"]@azure/storage-blob['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Blob storage is imported for Azure access",
                        "Azure Blob Storage is written via SDK",
                    ),
                ),
                (
                    r"require\(['\"]@google-cloud/storage['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud storage is imported for GCS access",
                        "GCS storage is written via Cloud SDK",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]@grpc/grpc-js['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "RPC communication is imported via gRPC",
                        "RPC service is accessed via gRPC",
                    ),
                ),
                (
                    r"require\(['\"]@grpc/proto-loader['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Protobuf definitions are loaded for gRPC",
                        "RPC service is accessed via gRPC",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]apollo-server['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL server is imported for API",
                        "GraphQL API is exposed for inbound requests",
                    ),
                ),
                (
                    r"require\(['\"]type-graphql['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL server is imported schema-first",
                        "GraphQL API is exposed via TypeGraphQL",
                    ),
                ),
                (
                    r"require\(['\"]@graphql-tools/",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL utilities are imported for tooling",
                        "GraphQL schema is accessed via graphql-tools",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]nodemailer['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email sending is imported via Nodemailer",
                        "Email message is sent via Nodemailer",
                    ),
                ),
                (
                    r"require\(['\"]@sendgrid/mail['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Transactional email is sent via SendGrid",
                        "Email is sent via SendGrid",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]ioredis['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is accessed via ioredis",
                        "Redis cache is written via ioredis",
                    ),
                ),
                (
                    r"require\(['\"]redis['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is accessed by client",
                        "Redis cache is written to with data",
                    ),
                ),
                (
                    r"require\(['\"]memcached['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached cache is accessed by client",
                        "Memcached cache is written via memcached",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"EventSource",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Server-sent events are received by client",
                        "SSE stream is connected for outbound events",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Event stream is received with content type",
                        "Event streams are accessed for server-sent events",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]ssh2-sftp-client['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP file transfer is imported",
                        "SFTP server is connected to with credentials",
                    ),
                ),
                (
                    r"require\(['\"]basic-ftp['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP file transfer is imported",
                        "FTP server is connected via basic-ftp",
                    ),
                ),
                (
                    r"require\(['\"]ftp['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP file transfer is imported",
                        "FTP server is connected via ftp",
                    ),
                ),
                (
                    r"SFTPWrapper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP file operations are handled by wrapper",
                        "SFTP server is written to via connection",
                    ),
                ),
                (
                    r"new Client\(\).*sftp",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP connection is created for file transfer",
                        "SFTP server is connected to with credentials",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"require\(['\"]node-cron['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cron tasks are scheduled by node-cron",
                        "Cron task is scheduled via node-cron",
                    ),
                ),
                (
                    r"require\(['\"]cron['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cron tasks are scheduled by scheduler",
                        "Cron task is scheduled via cron",
                    ),
                ),
                (
                    r"require\(['\"]agenda['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job scheduler is imported with MongoDB backend",
                        "Background jobs are scheduled via Agenda",
                    ),
                ),
            ],
        },
    },
)
