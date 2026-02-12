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
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]fs['\"]\)", Confidence.HIGH),
                (r"fs\.readFileSync", Confidence.HIGH),
                (r"require\(['\"]aws-sdk['\"]\)", Confidence.MEDIUM),
                (r"require\(['\"]multer['\"]\)", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"require\(['\"]@grpc/grpc-js['\"]\)", Confidence.HIGH),
                (r"require\(['\"]@grpc/proto-loader['\"]\)", Confidence.HIGH),
            ],
        },
    },
)
