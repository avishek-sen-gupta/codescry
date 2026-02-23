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
                        "HTTP server is imported via Fastify",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"from ['\"]koa['\"]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Koa",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"import axios",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP client is imported via axios",
                        "HTTP request is sent for outbound communication",
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
                        "SOAP client is imported for web services",
                        "SOAP service is accessed for communication",
                    ),
                ),
                (
                    r"import.*soap",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP integration is imported via library",
                        "SOAP service is accessed for communication",
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
                        "Kafka messaging is imported via client",
                        "Kafka broker is accessed for messaging",
                    ),
                ),
                (
                    r"from ['\"]amqplib['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP client is imported for messaging",
                        "AMQP broker is accessed for messaging",
                    ),
                ),
                (
                    r"from ['\"]bull['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job queue is imported with Redis",
                        "Job queue is managed for task processing",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-sqs['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SQS client is imported for messaging",
                        "SQS queue is accessed via SDK",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-sns['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SNS client is imported for notifications",
                        "SNS topic is accessed via SDK",
                    ),
                ),
                (
                    r"from ['\"]@google-cloud/pubsub['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Pub/Sub messaging is imported via client",
                        "Pub/Sub topic is managed for messaging",
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
                        "WebSocket server is imported via Socket.IO",
                        "WebSocket connection is accepted for inbound events",
                    ),
                ),
                (
                    r"from ['\"]ws['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "WebSocket client is imported via ws",
                        "WebSocket connection is managed for communication",
                    ),
                ),
                (
                    r"io\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket events are listened for inbound handling",
                        "Socket event is listened for inbound messages",
                    ),
                ),
                (
                    r"socket\.on\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Socket messages are handled by listener",
                        "Socket message is listened for inbound communication",
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
                        "Database entity is annotated with mapping",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"@Column\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is annotated with mapping",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"@PrimaryGeneratedColumn",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Primary key is annotated for auto-increment",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"@ManyToOne",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database association is annotated with ManyToOne",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"@OneToMany",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database association is annotated with OneToMany",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"@Repository\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database repository is annotated with decorator",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"from ['\"]typeorm['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database ORM is imported via TypeORM",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"from ['\"]@prisma/client['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database ORM is imported via Prisma Client",
                        "Database is queried with ORM client",
                    ),
                ),
                (
                    r"from ['\"]sequelize['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database ORM is imported via Sequelize",
                        "Database is accessed with ORM client",
                    ),
                ),
                (
                    r"from ['\"]mongoose['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB ODM is imported via Mongoose",
                        "MongoDB database is accessed with ODM",
                    ),
                ),
                (
                    r"prisma\.",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is queried by Prisma client",
                        "Database is queried with ORM client",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-dynamodb['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "DynamoDB client is imported for NoSQL",
                        "DynamoDB database is accessed via SDK",
                    ),
                ),
                (
                    r"from ['\"]firebase-admin['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Firebase services are imported for access",
                        "Firebase database is accessed with client",
                    ),
                ),
                (
                    r"from ['\"]neo4j-driver['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j client is imported for graph database",
                        "Graph database is accessed with driver",
                    ),
                ),
                (
                    r"neo4j\.driver\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j connection is created with driver",
                        "Graph database connection is opened with driver",
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
                        "File data is read from filesystem",
                        "File is read from filesystem",
                    ),
                ),
                (
                    r"fs\.writeFile",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File data is written to filesystem",
                        "File is written to filesystem",
                    ),
                ),
                (
                    r"from ['\"]@aws-sdk/client-s3['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 client is imported for object storage",
                        "S3 storage is accessed via SDK",
                    ),
                ),
                (
                    r"from ['\"]multer['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "File uploads are handled via Multer",
                        "File upload is accepted for inbound processing",
                    ),
                ),
                (
                    r"from ['\"]@azure/storage-blob['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Blob storage is imported for objects",
                        "Blob storage is accessed via cloud SDK",
                    ),
                ),
                (
                    r"from ['\"]@google-cloud/storage['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud storage is imported via client",
                        "Cloud storage is accessed via SDK",
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
                        "gRPC client is imported for communication",
                        "gRPC endpoint is accessed for communication",
                    ),
                ),
                (
                    r"from ['\"]@grpc/proto-loader['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Protobuf definitions are imported via proto-loader",
                        "gRPC endpoint is accessed for communication",
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
                        "GraphQL server is imported via Apollo",
                        "GraphQL API is exposed for inbound queries",
                    ),
                ),
                (
                    r"from ['\"]type-graphql['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL schema is imported via decorators",
                        "GraphQL API is exposed for inbound queries",
                    ),
                ),
                (
                    r"from ['\"]@graphql-tools/",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "GraphQL schema is imported via tools",
                        "GraphQL schema is managed with tools",
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
                        "Email client is imported via Nodemailer",
                        "Email message is sent via SMTP",
                    ),
                ),
                (
                    r"from ['\"]@sendgrid/mail['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email client is imported via SendGrid",
                        "Email message is sent via API",
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
                        "Redis client is imported via ioredis",
                        "Redis cache connection is opened with client",
                    ),
                ),
                (
                    r"from ['\"]redis['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis client is imported for caching",
                        "Redis cache connection is opened with client",
                    ),
                ),
                (
                    r"from ['\"]memcached['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached client is imported for caching",
                        "Memcached cache connection is opened with client",
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
                        "Server events are consumed via EventSource",
                        "SSE subscription is sent for outbound streaming",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SSE stream is indicated by content type",
                        "Server-sent events are streamed for communication",
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
                        "SFTP client is imported for file transfer",
                        "SFTP server connection is opened with client",
                    ),
                ),
                (
                    r"from ['\"]basic-ftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP client is imported via basic-ftp",
                        "FTP server connection is opened with client",
                    ),
                ),
                (
                    r"from ['\"]ftp['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP client is imported for transfer",
                        "FTP server connection is opened with client",
                    ),
                ),
                (
                    r"SFTPWrapper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP session is wrapped for operations",
                        "SFTP server connection is opened with client",
                    ),
                ),
                (
                    r"new Client\(\).*sftp",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP client is created for file transfer",
                        "SFTP server connection is opened with client",
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
                        "Cron scheduler is imported for tasks",
                        "Scheduled task is managed with cron",
                    ),
                ),
                (
                    r"from ['\"]cron['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Task scheduler is imported for cron jobs",
                        "Scheduled task is managed with cron",
                    ),
                ),
                (
                    r"from ['\"]agenda['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Job scheduler is imported with MongoDB",
                        "Scheduled jobs are managed with task scheduler",
                    ),
                ),
            ],
        },
    },
)
