"""JavaScript base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]fastify['\"]\)", Confidence.HIGH),
                (r"require\(['\"]koa['\"]\)", Confidence.MEDIUM),
                (r"require\(['\"]axios['\"]\)", Confidence.MEDIUM),
                (r"fetch\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]soap['\"]\)", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]kafkajs['\"]\)", Confidence.HIGH),
                (r"require\(['\"]amqplib['\"]\)", Confidence.HIGH),
                (r"require\(['\"]bull['\"]\)", Confidence.HIGH),
                (r"consumer\.run\(", Confidence.HIGH),
                (r"producer\.send\(", Confidence.HIGH),
                (r"require\(['\"]@aws-sdk/client-sqs['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@aws-sdk/client-sns['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@google-cloud/pubsub['\"]\)", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]socket.io['\"]\)", Confidence.HIGH),
                (r"require\(['\"]ws['\"]\)", Confidence.HIGH),
                (r"io\.on\(", Confidence.HIGH),
                (r"socket\.on\(", Confidence.HIGH),
                (r"new WebSocket\(", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]sequelize['\"]\)", Confidence.HIGH),
                (r"require\(['\"]mongoose['\"]\)", Confidence.HIGH),
                (r"require\(['\"]knex['\"]\)", Confidence.HIGH),
                (r"require\(['\"]mongodb['\"]\)", Confidence.HIGH),
                (r"mongoose\.Schema", Confidence.HIGH),
                (r"mongoose\.model\(", Confidence.HIGH),
                (r"Sequelize\.define\(", Confidence.HIGH),
                (r"require\(['\"]@aws-sdk/client-dynamodb['\"]\)", Confidence.HIGH),
                (r"require\(['\"]firebase-admin['\"]\)", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]fs['\"]\)", Confidence.HIGH),
                (r"fs\.readFileSync", Confidence.HIGH),
                (r"require\(['\"]aws-sdk['\"]\)", Confidence.MEDIUM),
                (r"require\(['\"]multer['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@azure/storage-blob['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@google-cloud/storage['\"]\)", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]@grpc/grpc-js['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@grpc/proto-loader['\"]\)", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]apollo-server['\"]\)", Confidence.HIGH),
                (r"require\(['\"]type-graphql['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@graphql-tools/", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]nodemailer['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@sendgrid/mail['\"]\)", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]ioredis['\"]\)", Confidence.HIGH),
                (r"require\(['\"]redis['\"]\)", Confidence.HIGH),
                (r"require\(['\"]memcached['\"]\)", Confidence.HIGH),
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
                (r"require\(['\"]ssh2-sftp-client['\"]\)", Confidence.HIGH),
                (r"require\(['\"]basic-ftp['\"]\)", Confidence.HIGH),
                (r"require\(['\"]ftp['\"]\)", Confidence.HIGH),
                (r"SFTPWrapper", Confidence.HIGH),
                (r"new Client\(\).*sftp", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]node-cron['\"]\)", Confidence.HIGH),
                (r"require\(['\"]cron['\"]\)", Confidence.HIGH),
                (r"require\(['\"]agenda['\"]\)", Confidence.HIGH),
            ],
        },
    },
)
