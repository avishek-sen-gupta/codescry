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
                        "Node.js Fastify framework require for HTTP server",
                        "This code uses Node.js Fastify to handle inbound HTTP server requests",
                    ),
                ),
                (
                    r"require\(['\"]koa['\"]\)",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Node.js Koa framework require for HTTP server",
                        "This code uses Node.js Koa to handle inbound HTTP server requests",
                    ),
                ),
                (
                    r"require\(['\"]axios['\"]\)",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Axios library require for HTTP client requests",
                        "This code uses Node.js Axios to send outbound HTTP requests",
                    ),
                ),
                (
                    r"fetch\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "JavaScript fetch API call for HTTP client requests",
                        "This code uses JavaScript fetch to send outbound HTTP requests",
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
                        "Node.js soap library require for SOAP web service integration",
                        "This code uses Node.js soap to interact with SOAP web services",
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
                        "Node.js KafkaJS library require for Kafka messaging",
                        "This code uses Node.js KafkaJS to interact with a Kafka message broker",
                    ),
                ),
                (
                    r"require\(['\"]amqplib['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js amqplib library require for AMQP/RabbitMQ messaging",
                        "This code uses Node.js amqplib to interact with an AMQP message broker",
                    ),
                ),
                (
                    r"require\(['\"]bull['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js Bull library require for Redis-backed job queue",
                        "This code uses Node.js Bull to interact with a job queue",
                    ),
                ),
                (
                    r"consumer\.run\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Node.js KafkaJS consumer.run for Kafka message consumption",
                        "This code uses Node.js KafkaJS to receive inbound Kafka messages",
                    ),
                ),
                (
                    r"producer\.send\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js KafkaJS producer.send for Kafka message publishing",
                        "This code uses Node.js KafkaJS to produce outgoing Kafka messages",
                    ),
                ),
                (
                    r"require\(['\"]@aws-sdk/client-sqs['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js AWS SDK SQS client require for SQS messaging",
                        "This code uses Node.js AWS SDK to interact with an SQS message queue",
                    ),
                ),
                (
                    r"require\(['\"]@aws-sdk/client-sns['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js AWS SDK SNS client require for SNS notifications",
                        "This code uses Node.js AWS SDK to interact with SNS notifications",
                    ),
                ),
                (
                    r"require\(['\"]@google-cloud/pubsub['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js Google Cloud Pub/Sub require for GCP messaging",
                        "This code uses Node.js Google Cloud to interact with Pub/Sub messaging",
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
                        "Node.js Socket.IO library require for WebSocket server",
                        "This code uses Node.js Socket.IO to handle inbound WebSocket connections",
                    ),
                ),
                (
                    r"require\(['\"]ws['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js ws library require for WebSocket communication",
                        "This code uses Node.js ws to interact with WebSocket connections",
                    ),
                ),
                (
                    r"io\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Node.js Socket.IO io.on for event listener registration",
                        "This code uses Node.js Socket.IO to listen for inbound socket events",
                    ),
                ),
                (
                    r"socket\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Node.js WebSocket socket.on for event listener registration",
                        "This code uses Node.js to listen for inbound WebSocket events",
                    ),
                ),
                (
                    r"new WebSocket\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JavaScript WebSocket constructor for outbound WebSocket client",
                        "This code uses JavaScript to connect to an outbound WebSocket server",
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
                        "Node.js Sequelize ORM require for SQL database access",
                        "This code uses Node.js Sequelize to query a relational database",
                    ),
                ),
                (
                    r"require\(['\"]mongoose['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Mongoose ODM require for MongoDB",
                        "This code uses Node.js Mongoose to interact with a MongoDB database",
                    ),
                ),
                (
                    r"require\(['\"]knex['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Knex query builder require for SQL database access",
                        "This code uses Node.js Knex to query a relational database",
                    ),
                ),
                (
                    r"require\(['\"]mongodb['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js MongoDB driver require for MongoDB database access",
                        "This code uses Node.js MongoDB driver to query a MongoDB database",
                    ),
                ),
                (
                    r"mongoose\.Schema",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Mongoose Schema definition for MongoDB data modelling",
                        "This code uses Node.js Mongoose to query a MongoDB database schema",
                    ),
                ),
                (
                    r"mongoose\.model\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Mongoose model registration for MongoDB collection",
                        "This code uses Node.js Mongoose to query a MongoDB collection model",
                    ),
                ),
                (
                    r"Sequelize\.define\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Sequelize define call for SQL model definition",
                        "This code uses Node.js Sequelize to query a relational database model",
                    ),
                ),
                (
                    r"require\(['\"]@aws-sdk/client-dynamodb['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js AWS SDK DynamoDB client require for DynamoDB access",
                        "This code uses Node.js AWS SDK to query a DynamoDB database",
                    ),
                ),
                (
                    r"require\(['\"]firebase-admin['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Firebase Admin SDK require for Firebase database access",
                        "This code uses Node.js Firebase Admin to query a Firebase database",
                    ),
                ),
                (
                    r"require\(['\"]neo4j-driver['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Neo4j driver require for graph database access",
                        "This code uses Node.js Neo4j driver to query a graph database",
                    ),
                ),
                (
                    r"neo4j\.driver\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Neo4j driver instantiation for graph database connectivity",
                        "This code uses Node.js Neo4j to connect to a graph database",
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
                        "Node.js fs module require for file system access",
                        "This code uses Node.js fs to interact with the file system",
                    ),
                ),
                (
                    r"fs\.readFileSync",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js fs.readFileSync call for synchronous file reading",
                        "This code uses Node.js fs to read from the file system",
                    ),
                ),
                (
                    r"require\(['\"]aws-sdk['\"]\)",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js AWS SDK require for AWS service access",
                        "This code uses Node.js AWS SDK to interact with AWS cloud services",
                    ),
                ),
                (
                    r"require\(['\"]multer['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Node.js Multer middleware require for file upload handling",
                        "This code uses Node.js Multer to accept inbound file uploads",
                    ),
                ),
                (
                    r"require\(['\"]@azure/storage-blob['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Azure Blob Storage require for blob storage access",
                        "This code uses Node.js Azure SDK to write to Azure Blob Storage",
                    ),
                ),
                (
                    r"require\(['\"]@google-cloud/storage['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js Google Cloud Storage require for GCS object storage access",
                        "This code uses Node.js Google Cloud to write to GCS object storage",
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
                        "Node.js gRPC library require for RPC communication",
                        "This code uses Node.js gRPC to interact with a remote procedure call service",
                    ),
                ),
                (
                    r"require\(['\"]@grpc/proto-loader['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js gRPC proto-loader require for Protobuf definition loading",
                        "This code uses Node.js gRPC to interact with Protobuf-defined services",
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
                        "Node.js Apollo Server require for GraphQL server",
                        "This code uses Node.js Apollo Server to expose inbound GraphQL APIs",
                    ),
                ),
                (
                    r"require\(['\"]type-graphql['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Node.js TypeGraphQL require for GraphQL schema-first server",
                        "This code uses Node.js TypeGraphQL to expose inbound GraphQL APIs",
                    ),
                ),
                (
                    r"require\(['\"]@graphql-tools/",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js graphql-tools require for GraphQL utilities",
                        "This code uses Node.js graphql-tools to interact with GraphQL schemas",
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
                        "Node.js Nodemailer require for email sending",
                        "This code uses Node.js Nodemailer to send outbound email messages",
                    ),
                ),
                (
                    r"require\(['\"]@sendgrid/mail['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js SendGrid mail require for transactional email sending",
                        "This code uses Node.js SendGrid to send outbound transactional emails",
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
                        "Node.js ioredis require for Redis cache access",
                        "This code uses Node.js ioredis to write to a Redis cache",
                    ),
                ),
                (
                    r"require\(['\"]redis['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js redis client require for Redis cache access",
                        "This code uses Node.js redis to write to a Redis cache",
                    ),
                ),
                (
                    r"require\(['\"]memcached['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js memcached client require for Memcached cache access",
                        "This code uses Node.js memcached to write to a Memcached cache",
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
                        "JavaScript EventSource API for server-sent event client",
                        "This code uses JavaScript EventSource to connect to outbound SSE streams",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "JavaScript SSE content type for event stream",
                        "This code uses JavaScript to interact with server-sent event streams",
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
                        "Node.js ssh2-sftp-client require for SFTP file transfer",
                        "This code uses Node.js ssh2-sftp-client to connect to an SFTP server",
                    ),
                ),
                (
                    r"require\(['\"]basic-ftp['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js basic-ftp require for FTP file transfer",
                        "This code uses Node.js basic-ftp to connect to an FTP server",
                    ),
                ),
                (
                    r"require\(['\"]ftp['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js ftp require for FTP file transfer",
                        "This code uses Node.js ftp to connect to an FTP server",
                    ),
                ),
                (
                    r"SFTPWrapper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js ssh2 SFTPWrapper for SFTP file operations",
                        "This code uses Node.js ssh2 to write to an SFTP server",
                    ),
                ),
                (
                    r"new Client\(\).*sftp",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Node.js SFTP Client instantiation for SFTP connectivity",
                        "This code uses Node.js to connect to an SFTP server",
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
                        "Node.js node-cron require for cron-based task scheduling",
                        "This code uses Node.js node-cron to interact with scheduled cron tasks",
                    ),
                ),
                (
                    r"require\(['\"]cron['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js cron require for cron-based task scheduling",
                        "This code uses Node.js cron to interact with scheduled cron tasks",
                    ),
                ),
                (
                    r"require\(['\"]agenda['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Node.js Agenda require for MongoDB-backed job scheduling",
                        "This code uses Node.js Agenda to interact with scheduled background jobs",
                    ),
                ),
            ],
        },
    },
)
