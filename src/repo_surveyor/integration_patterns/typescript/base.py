"""TypeScript base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"from ['\"]fastify['\"]", Confidence.HIGH),
                (r"from ['\"]koa['\"]", Confidence.MEDIUM),
                (r"import axios", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"from ['\"]soap['\"]", Confidence.MEDIUM),
                (r"import.*soap", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"from ['\"]kafkajs['\"]", Confidence.HIGH),
                (r"from ['\"]amqplib['\"]", Confidence.HIGH),
                (r"from ['\"]bull['\"]", Confidence.HIGH),
                (r"from ['\"]@aws-sdk/client-sqs['\"]", Confidence.HIGH),
                (r"from ['\"]@aws-sdk/client-sns['\"]", Confidence.HIGH),
                (r"from ['\"]@google-cloud/pubsub['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"from ['\"]socket\.io['\"]", Confidence.HIGH),
                (r"from ['\"]ws['\"]", Confidence.HIGH),
                (r"io\.on\(", Confidence.HIGH),
                (r"socket\.on\(", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"@Entity\(", Confidence.HIGH),
                (r"@Column\(", Confidence.MEDIUM),
                (r"@PrimaryGeneratedColumn", Confidence.HIGH),
                (r"@ManyToOne", Confidence.MEDIUM),
                (r"@OneToMany", Confidence.MEDIUM),
                (r"@Repository\(", Confidence.HIGH),
                (r"from ['\"]typeorm['\"]", Confidence.HIGH),
                (r"from ['\"]@prisma/client['\"]", Confidence.HIGH),
                (r"from ['\"]sequelize['\"]", Confidence.HIGH),
                (r"from ['\"]mongoose['\"]", Confidence.HIGH),
                (r"prisma\.", Confidence.HIGH),
                (r"from ['\"]@aws-sdk/client-dynamodb['\"]", Confidence.HIGH),
                (r"from ['\"]firebase-admin['\"]", Confidence.HIGH),
                (r"from ['\"]neo4j-driver['\"]", Confidence.HIGH),
                (r"neo4j\.driver\(", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"fs\.readFile", Confidence.HIGH),
                (r"fs\.writeFile", Confidence.HIGH),
                (r"from ['\"]@aws-sdk/client-s3['\"]", Confidence.HIGH),
                (r"from ['\"]multer['\"]", Confidence.HIGH),
                (r"from ['\"]@azure/storage-blob['\"]", Confidence.HIGH),
                (r"from ['\"]@google-cloud/storage['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"from ['\"]@grpc/grpc-js['\"]", Confidence.HIGH),
                (r"from ['\"]@grpc/proto-loader['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"from ['\"]apollo-server['\"]", Confidence.HIGH),
                (r"from ['\"]type-graphql['\"]", Confidence.HIGH),
                (r"from ['\"]@graphql-tools/", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"from ['\"]nodemailer['\"]", Confidence.HIGH),
                (r"from ['\"]@sendgrid/mail['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"from ['\"]ioredis['\"]", Confidence.HIGH),
                (r"from ['\"]redis['\"]", Confidence.HIGH),
                (r"from ['\"]memcached['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"EventSource", Confidence.MEDIUM),
                (r"text/event-stream", Confidence.MEDIUM),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"from ['\"]ssh2-sftp-client['\"]", Confidence.HIGH),
                (r"from ['\"]basic-ftp['\"]", Confidence.HIGH),
                (r"from ['\"]ftp['\"]", Confidence.HIGH),
                (r"SFTPWrapper", Confidence.HIGH),
                (r"new Client\(\).*sftp", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"from ['\"]node-cron['\"]", Confidence.HIGH),
                (r"from ['\"]cron['\"]", Confidence.HIGH),
                (r"from ['\"]agenda['\"]", Confidence.HIGH),
            ],
        },
    },
)
