"""TypeScript base integration patterns."""

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
                    r"from ['\"]fastify['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Fastify import for HTTP server framework",
                        "This code uses TypeScript Fastify to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"from ['\"]koa['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Koa import for HTTP server framework",
                        "This code uses TypeScript Koa to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"import axios",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript axios import for HTTP client",
                        "This code uses TypeScript axios to send outbound HTTP requests",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]soap['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript soap import for SOAP web service integration",
                        "This code uses TypeScript soap to interact with a SOAP web service",
                    ),
                ),
                (
                    r"import.*soap",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript soap library import for SOAP web service integration",
                        "This code uses TypeScript soap to interact with a SOAP web service",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]kafkajs['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript KafkaJS import for Kafka messaging client",
                        "This code uses TypeScript KafkaJS to interact with a Kafka broker",
                    ),
                ),
                (
                    r"from ['\"]amqplib['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript amqplib import for AMQP messaging client",
                        "This code uses TypeScript amqplib to interact with an AMQP broker",
                    ),
                ),
                (
                    r"from ['\"]bull['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript Bull import for Redis-backed job queue",
                        "This code uses TypeScript Bull to interact with a job queue",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-sqs['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript AWS SDK SQS client import for queue messaging",
                        "This code uses TypeScript AWS SDK to interact with an SQS queue",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-sns['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript AWS SDK SNS client import for pub/sub notifications",
                        "This code uses TypeScript AWS SDK to interact with an SNS topic",
                    ),
                ),
                (
                    r"from ['\"]@google-cloud/pubsub['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript Google Cloud Pub/Sub client import for messaging",
                        "This code uses TypeScript Google Cloud to interact with a Pub/Sub topic",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]socket\.io['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Socket.IO import for real-time WebSocket server",
                        "This code uses TypeScript Socket.IO to accept inbound WebSocket connections",
                    ),
                ),
                (
                    r"from ['\"]ws['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript ws import for WebSocket client and server",
                        "This code uses TypeScript ws to interact with a WebSocket connection",
                    ),
                ),
                (
                    r"io\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Socket.IO io.on() listener accepting inbound events",
                        "This code uses TypeScript Socket.IO to listen for inbound socket events",
                    ),
                ),
                (
                    r"socket\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript socket.on() listener handling inbound socket messages",
                        "This code uses TypeScript to listen for inbound socket messages",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"@Entity\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript TypeORM @Entity decorator mapping a database entity",
                        "This code uses TypeScript TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"@Column\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript TypeORM @Column decorator mapping a database column",
                        "This code uses TypeScript TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"@PrimaryGeneratedColumn",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript TypeORM @PrimaryGeneratedColumn decorator for auto-increment primary key",
                        "This code uses TypeScript TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"@ManyToOne",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript TypeORM @ManyToOne decorator for relational database association",
                        "This code uses TypeScript TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"@OneToMany",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript TypeORM @OneToMany decorator for relational database association",
                        "This code uses TypeScript TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"@Repository\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript TypeORM @Repository decorator for database repository",
                        "This code uses TypeScript TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"from ['\"]typeorm['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript TypeORM import for ORM database integration",
                        "This code uses TypeScript TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"from ['\"]@prisma/client['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Prisma Client import for database ORM",
                        "This code uses TypeScript Prisma to query a relational database",
                    ),
                ),
                (
                    r"from ['\"]sequelize['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Sequelize import for ORM database integration",
                        "This code uses TypeScript Sequelize to interact with a relational database",
                    ),
                ),
                (
                    r"from ['\"]mongoose['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Mongoose import for MongoDB ODM integration",
                        "This code uses TypeScript Mongoose to interact with a MongoDB database",
                    ),
                ),
                (
                    r"prisma\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Prisma client call querying a database",
                        "This code uses TypeScript Prisma to query a relational database",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-dynamodb['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript AWS SDK DynamoDB client import for NoSQL database",
                        "This code uses TypeScript AWS SDK to interact with a DynamoDB database",
                    ),
                ),
                (
                    r"from ['\"]firebase-admin['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Firebase Admin import for Firestore/RTDB access",
                        "This code uses TypeScript Firebase to interact with a Firebase database",
                    ),
                ),
                (
                    r"from ['\"]neo4j-driver['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript neo4j-driver import for Neo4j graph database client",
                        "This code uses TypeScript Neo4j driver to interact with a graph database",
                    ),
                ),
                (
                    r"neo4j\.driver\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript neo4j.driver() call creating a Neo4j database connection",
                        "This code uses TypeScript Neo4j driver to connect to a graph database",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"fs\.readFile",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript fs.readFile call reading data from a file",
                        "This code uses TypeScript Node.js fs to read from a file",
                    ),
                ),
                (
                    r"fs\.writeFile",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript fs.writeFile call writing data to a file",
                        "This code uses TypeScript Node.js fs to write to a file",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-s3['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript AWS SDK S3 client import for object storage",
                        "This code uses TypeScript AWS SDK to interact with S3 object storage",
                    ),
                ),
                (
                    r"from ['\"]multer['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Multer import for inbound multipart file upload handling",
                        "This code uses TypeScript Multer to accept inbound file uploads",
                    ),
                ),
                (
                    r"from ['\"]@azure/storage-blob['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Azure Storage Blob client import for object storage",
                        "This code uses TypeScript Azure SDK to interact with Blob storage",
                    ),
                ),
                (
                    r"from ['\"]@google-cloud/storage['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Google Cloud Storage client import for object storage",
                        "This code uses TypeScript Google Cloud to interact with Cloud Storage",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@grpc/grpc-js['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript gRPC-js import for gRPC client and server",
                        "This code uses TypeScript gRPC to interact with a gRPC endpoint",
                    ),
                ),
                (
                    r"from ['\"]@grpc/proto-loader['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript gRPC proto-loader import for loading protobuf definitions",
                        "This code uses TypeScript gRPC to interact with a gRPC endpoint",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]apollo-server['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Apollo Server import for GraphQL server",
                        "This code uses TypeScript Apollo Server to expose an inbound GraphQL API",
                    ),
                ),
                (
                    r"from ['\"]type-graphql['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript type-graphql import for decorator-based GraphQL schema",
                        "This code uses TypeScript type-graphql to expose an inbound GraphQL API",
                    ),
                ),
                (
                    r"from ['\"]@graphql-tools/",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript graphql-tools import for GraphQL schema utilities",
                        "This code uses TypeScript graphql-tools to interact with a GraphQL schema",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]nodemailer['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript Nodemailer import for email sending",
                        "This code uses TypeScript Nodemailer to send outbound email messages",
                    ),
                ),
                (
                    r"from ['\"]@sendgrid/mail['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript SendGrid mail client import for email sending",
                        "This code uses TypeScript SendGrid to send outbound email messages",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]ioredis['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript ioredis import for Redis cache client",
                        "This code uses TypeScript ioredis to connect to a Redis cache",
                    ),
                ),
                (
                    r"from ['\"]redis['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript redis import for Redis cache client",
                        "This code uses TypeScript redis to connect to a Redis cache",
                    ),
                ),
                (
                    r"from ['\"]memcached['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript memcached import for Memcached client",
                        "This code uses TypeScript memcached to connect to a Memcached cache",
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
                        "TypeScript EventSource API consuming server-sent events",
                        "This code uses TypeScript EventSource to send outbound SSE subscription requests",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript text/event-stream content type for SSE streaming",
                        "This code uses TypeScript to interact with server-sent event streaming",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]ssh2-sftp-client['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript ssh2-sftp-client import for SFTP file transfer client",
                        "This code uses TypeScript ssh2-sftp-client to connect to an SFTP server",
                    ),
                ),
                (
                    r"from ['\"]basic-ftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript basic-ftp import for FTP client",
                        "This code uses TypeScript basic-ftp to connect to an FTP server",
                    ),
                ),
                (
                    r"from ['\"]ftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript ftp import for FTP client",
                        "This code uses TypeScript ftp to connect to an FTP server",
                    ),
                ),
                (
                    r"SFTPWrapper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript SFTPWrapper class for SFTP session operations",
                        "This code uses TypeScript ssh2 SFTPWrapper to connect to an SFTP server",
                    ),
                ),
                (
                    r"new Client\(\).*sftp",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "TypeScript SFTP Client instantiation for file transfer",
                        "This code uses TypeScript to connect to an SFTP server",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]node-cron['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript node-cron import for cron-based task scheduling",
                        "This code uses TypeScript node-cron to interact with scheduled tasks",
                    ),
                ),
                (
                    r"from ['\"]cron['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript cron import for cron-based task scheduling",
                        "This code uses TypeScript cron to interact with scheduled tasks",
                    ),
                ),
                (
                    r"from ['\"]agenda['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TypeScript Agenda import for MongoDB-backed job scheduling",
                        "This code uses TypeScript Agenda to interact with scheduled jobs",
                    ),
                ),
            ],
        },
    },
)
