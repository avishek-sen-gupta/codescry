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
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"fs\.readFile", Confidence.HIGH),
                (r"fs\.writeFile", Confidence.HIGH),
                (r"from ['\"]@aws-sdk/client-s3['\"]", Confidence.HIGH),
                (r"from ['\"]multer['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"from ['\"]@grpc/grpc-js['\"]", Confidence.HIGH),
                (r"from ['\"]@grpc/proto-loader['\"]", Confidence.HIGH),
            ],
        },
    },
)
