"""JavaScript integration patterns."""

from .types import Confidence, IntegrationType

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"require\(['\"]express['\"]\)", Confidence.HIGH),
            (r"require\(['\"]fastify['\"]\)", Confidence.HIGH),
            (r"require\(['\"]koa['\"]\)", Confidence.MEDIUM),
            (r"require\(['\"]axios['\"]\)", Confidence.MEDIUM),
            (r"app\.get\(", Confidence.HIGH),
            (r"app\.post\(", Confidence.HIGH),
            (r"app\.put\(", Confidence.HIGH),
            (r"app\.delete\(", Confidence.HIGH),
            (r"router\.get\(", Confidence.HIGH),
            (r"router\.post\(", Confidence.HIGH),
            (r"fetch\(", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            (r"require\(['\"]soap['\"]\)", Confidence.MEDIUM),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"require\(['\"]kafkajs['\"]\)", Confidence.HIGH),
            (r"require\(['\"]amqplib['\"]\)", Confidence.HIGH),
            (r"require\(['\"]bull['\"]\)", Confidence.HIGH),
            (r"consumer\.run\(", Confidence.HIGH),
            (r"producer\.send\(", Confidence.HIGH),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"require\(['\"]socket.io['\"]\)", Confidence.HIGH),
            (r"require\(['\"]ws['\"]\)", Confidence.HIGH),
            (r"io\.on\(", Confidence.HIGH),
            (r"socket\.on\(", Confidence.HIGH),
            (r"new WebSocket\(", Confidence.HIGH),
        ],
    },
    IntegrationType.DATABASE: {
        "patterns": [
            (r"require\(['\"]sequelize['\"]\)", Confidence.HIGH),
            (r"require\(['\"]mongoose['\"]\)", Confidence.HIGH),
            (r"require\(['\"]knex['\"]\)", Confidence.HIGH),
            (r"require\(['\"]mongodb['\"]\)", Confidence.HIGH),
            (r"mongoose\.Schema", Confidence.HIGH),
            (r"mongoose\.model\(", Confidence.HIGH),
            (r"Sequelize\.define\(", Confidence.HIGH),
        ],
    },
}
