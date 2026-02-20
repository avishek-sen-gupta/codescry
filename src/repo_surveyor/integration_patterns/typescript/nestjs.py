"""NestJS framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="NestJS",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@Controller\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Delete\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Patch\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Body\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Param\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Query\(", Confidence.HIGH, SignalDirection.INWARD),
                (
                    r"from ['\"]@nestjs/common['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@nestjs/typeorm['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"from ['\"]@nestjs/mongoose['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"from ['\"]@nestjs/sequelize['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"TypeOrmModule", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MongooseModule", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SequelizeModule", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PrismaService", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PrismaClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"InjectRepository\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"InjectModel\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@nestjs/prisma", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"from ['\"]nest-neo4j['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"Neo4jService", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@MessagePattern\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@EventPattern\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"@WebSocketGateway", Confidence.HIGH, SignalDirection.INWARD),
                (r"@SubscribeMessage", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@nestjs/graphql['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r"@Query\(\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Mutation\(\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Resolver\(\)", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"@nestjs-modules/mailer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MailerService", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"@nestjs/cache-manager", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"CacheModule", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"@Sse\(\)", Confidence.HIGH, SignalDirection.INWARD),
                (r"Observable<MessageEvent>", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"@Cron\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"from ['\"]@nestjs/schedule['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"ScheduleModule", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
