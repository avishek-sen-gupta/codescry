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
                (r"from ['\"]fastify['\"]", Confidence.HIGH, SignalDirection.INWARD),
                (r"from ['\"]koa['\"]", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"import axios", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"from ['\"]soap['\"]", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"import.*soap", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"from ['\"]kafkajs['\"]", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from ['\"]amqplib['\"]", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from ['\"]bull['\"]", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"from ['\"]@aws-sdk/client-sqs['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"from ['\"]@aws-sdk/client-sns['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"from ['\"]@google-cloud/pubsub['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"from ['\"]socket\.io['\"]", Confidence.HIGH, SignalDirection.INWARD),
                (r"from ['\"]ws['\"]", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"io\.on\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"socket\.on\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"@Entity\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Column\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@PrimaryGeneratedColumn", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@ManyToOne", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@OneToMany", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@Repository\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from ['\"]typeorm['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"from ['\"]@prisma/client['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"from ['\"]sequelize['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from ['\"]mongoose['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"prisma\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"from ['\"]@aws-sdk/client-dynamodb['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"from ['\"]firebase-admin['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"from ['\"]neo4j-driver['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"neo4j\.driver\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"fs\.readFile", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"fs\.writeFile", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"from ['\"]@aws-sdk/client-s3['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"from ['\"]multer['\"]", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"from ['\"]@azure/storage-blob['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"from ['\"]@google-cloud/storage['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@grpc/grpc-js['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"from ['\"]@grpc/proto-loader['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]apollo-server['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (
                    r"from ['\"]type-graphql['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (
                    r"from ['\"]@graphql-tools/",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]nodemailer['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"from ['\"]@sendgrid/mail['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"from ['\"]ioredis['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from ['\"]redis['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from ['\"]memcached['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"EventSource", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"text/event-stream", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]ssh2-sftp-client['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"from ['\"]basic-ftp['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from ['\"]ftp['\"]", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SFTPWrapper", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"new Client\(\).*sftp", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]node-cron['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"from ['\"]cron['\"]", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"from ['\"]agenda['\"]", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
