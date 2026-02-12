"""TypeScript integration patterns."""

from .types import Confidence, IntegrationType

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
}

FRAMEWORK_PATTERNS = {
    "NestJS": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@Controller\(", Confidence.HIGH),
                (r"@Get\(", Confidence.HIGH),
                (r"@Post\(", Confidence.HIGH),
                (r"@Put\(", Confidence.HIGH),
                (r"@Delete\(", Confidence.HIGH),
                (r"@Patch\(", Confidence.HIGH),
                (r"@Body\(", Confidence.HIGH),
                (r"@Param\(", Confidence.HIGH),
                (r"@Query\(", Confidence.HIGH),
                (r"from ['\"]@nestjs/common['\"]", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r"@MessagePattern\(", Confidence.HIGH),
                (r"@EventPattern\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"@WebSocketGateway", Confidence.HIGH),
                (r"@SubscribeMessage", Confidence.HIGH),
            ],
        },
    },
    "Express": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"from ['\"]express['\"]", Confidence.HIGH),
                (r"\w+\.get\(", Confidence.HIGH),
                (r"\w+\.post\(", Confidence.HIGH),
                (r"\w+\.put\(", Confidence.HIGH),
                (r"\w+\.delete\(", Confidence.HIGH),
            ],
        },
    },
    "Angular": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"from ['\"]@angular/common/http['\"]", Confidence.HIGH),
                (r"HttpClient", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"from ['\"]@angular/.*websocket", Confidence.MEDIUM),
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
                (r"from ['\"]next/server['\"]", Confidence.HIGH),
            ],
        },
    },
}
