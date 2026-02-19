"""JavaScript base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey, SignalDirection

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]fastify['\"]\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"require\(['\"]koa['\"]\)", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"require\(['\"]axios['\"]\)", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"fetch\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]soap['\"]\)", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]kafkajs['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"require\(['\"]amqplib['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"require\(['\"]bull['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"consumer\.run\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"producer\.send\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]@aws-sdk/client-sqs['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"require\(['\"]@aws-sdk/client-sns['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"require\(['\"]@google-cloud/pubsub['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]socket.io['\"]\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"require\(['\"]ws['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"io\.on\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"socket\.on\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"new WebSocket\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]sequelize['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]mongoose['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]knex['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]mongodb['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"mongoose\.Schema", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"mongoose\.model\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Sequelize\.define\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]@aws-sdk/client-dynamodb['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]firebase-admin['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]neo4j-driver['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"neo4j\.driver\(", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]fs['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"fs\.readFileSync", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]aws-sdk['\"]\)", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"require\(['\"]multer['\"]\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"require\(['\"]@azure/storage-blob['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]@google-cloud/storage['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]@grpc/grpc-js['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"require\(['\"]@grpc/proto-loader['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]apollo-server['\"]\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"require\(['\"]type-graphql['\"]\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"require\(['\"]@graphql-tools/", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]nodemailer['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]@sendgrid/mail['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]ioredis['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]redis['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]memcached['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
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
                (r"require\(['\"]ssh2-sftp-client['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]basic-ftp['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"require\(['\"]ftp['\"]\)", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SFTPWrapper", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"new Client\(\).*sftp", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]node-cron['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"require\(['\"]cron['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"require\(['\"]agenda['\"]\)", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
