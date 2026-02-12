"""JavaScript integration patterns."""

from .types import Confidence, IntegrationType

BASE_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"require\(['\"]fastify['\"]\)", Confidence.HIGH),
            (r"require\(['\"]koa['\"]\)", Confidence.MEDIUM),
            (r"require\(['\"]axios['\"]\)", Confidence.MEDIUM),
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
    IntegrationType.FILE_IO: {
        "patterns": [
            (r"require\(['\"]fs['\"]\)", Confidence.HIGH),
            (r"fs\.readFileSync", Confidence.HIGH),
            (r"require\(['\"]aws-sdk['\"]\)", Confidence.MEDIUM),
            (r"require\(['\"]multer['\"]\)", Confidence.HIGH),
        ],
    },
    IntegrationType.GRPC: {
        "patterns": [
            (r"require\(['\"]@grpc/grpc-js['\"]\)", Confidence.HIGH),
            (r"require\(['\"]@grpc/proto-loader['\"]\)", Confidence.HIGH),
        ],
    },
}

FRAMEWORK_PATTERNS = {
    "Express": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"require\(['\"]express['\"]\)", Confidence.HIGH),
                (r"\w+\.get\(", Confidence.HIGH),
                (r"\w+\.post\(", Confidence.HIGH),
                (r"\w+\.put\(", Confidence.HIGH),
                (r"\w+\.delete\(", Confidence.HIGH),
            ],
        },
    },
    "Next.js": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"NextApiRequest", Confidence.HIGH),
                (r"NextApiResponse", Confidence.HIGH),
                (r"getServerSideProps", Confidence.HIGH),
                (r"getStaticProps", Confidence.HIGH),
                (r"require\(['\"]next/server['\"]\)", Confidence.HIGH),
            ],
        },
    },
}
