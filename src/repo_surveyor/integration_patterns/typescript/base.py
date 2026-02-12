"""TypeScript base integration patterns."""

from ..types import Confidence, IntegrationType

BASE_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"from ['\"]fastify['\"]", Confidence.HIGH),
            (r"from ['\"]koa['\"]", Confidence.MEDIUM),
            (r"import axios", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            (r"from ['\"]soap['\"]", Confidence.MEDIUM),
            (r"import.*soap", Confidence.MEDIUM),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"from ['\"]kafkajs['\"]", Confidence.HIGH),
            (r"from ['\"]amqplib['\"]", Confidence.HIGH),
            (r"from ['\"]bull['\"]", Confidence.HIGH),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"from ['\"]socket\.io['\"]", Confidence.HIGH),
            (r"from ['\"]ws['\"]", Confidence.HIGH),
            (r"io\.on\(", Confidence.HIGH),
            (r"socket\.on\(", Confidence.HIGH),
        ],
    },
    IntegrationType.DATABASE: {
        "patterns": [
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
        ],
    },
    IntegrationType.FILE_IO: {
        "patterns": [
            (r"fs\.readFile", Confidence.HIGH),
            (r"fs\.writeFile", Confidence.HIGH),
            (r"from ['\"]@aws-sdk/client-s3['\"]", Confidence.HIGH),
            (r"from ['\"]multer['\"]", Confidence.HIGH),
        ],
    },
    IntegrationType.GRPC: {
        "patterns": [
            (r"from ['\"]@grpc/grpc-js['\"]", Confidence.HIGH),
            (r"from ['\"]@grpc/proto-loader['\"]", Confidence.HIGH),
        ],
    },
}
