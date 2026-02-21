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
                (
                    r"@Controller\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Controller decorator defining an inbound HTTP controller",
                        "This code uses NestJS to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"@Get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Get decorator handling inbound GET HTTP requests",
                        "This code uses NestJS to handle inbound GET HTTP requests",
                    ),
                ),
                (
                    r"@Post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Post decorator handling inbound POST HTTP requests",
                        "This code uses NestJS to handle inbound POST HTTP requests",
                    ),
                ),
                (
                    r"@Put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Put decorator handling inbound PUT HTTP requests",
                        "This code uses NestJS to handle inbound PUT HTTP requests",
                    ),
                ),
                (
                    r"@Delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Delete decorator handling inbound DELETE HTTP requests",
                        "This code uses NestJS to handle inbound DELETE HTTP requests",
                    ),
                ),
                (
                    r"@Patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Patch decorator handling inbound PATCH HTTP requests",
                        "This code uses NestJS to handle inbound PATCH HTTP requests",
                    ),
                ),
                (
                    r"@Body\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Body decorator extracting inbound request body",
                        "This code uses NestJS to receive inbound HTTP request body data",
                    ),
                ),
                (
                    r"@Param\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Param decorator extracting inbound route parameters",
                        "This code uses NestJS to receive inbound HTTP route parameters",
                    ),
                ),
                (
                    r"@Query\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Query decorator extracting inbound query parameters",
                        "This code uses NestJS to receive inbound HTTP query parameters",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/common['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "NestJS @nestjs/common import for core framework components",
                        "This code uses NestJS to integrate with the NestJS framework",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@nestjs/typeorm['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS TypeORM module import for relational database integration",
                        "This code uses NestJS TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/mongoose['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS Mongoose module import for MongoDB integration",
                        "This code uses NestJS Mongoose to interact with a MongoDB database",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/sequelize['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS Sequelize module import for relational database integration",
                        "This code uses NestJS Sequelize to interact with a relational database",
                    ),
                ),
                (
                    r"TypeOrmModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS TypeOrmModule class configuring TypeORM database integration",
                        "This code uses NestJS TypeORM to interact with a relational database",
                    ),
                ),
                (
                    r"MongooseModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS MongooseModule class configuring Mongoose MongoDB integration",
                        "This code uses NestJS Mongoose to interact with a MongoDB database",
                    ),
                ),
                (
                    r"SequelizeModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS SequelizeModule class configuring Sequelize database integration",
                        "This code uses NestJS Sequelize to interact with a relational database",
                    ),
                ),
                (
                    r"PrismaService",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS PrismaService class providing Prisma ORM database access",
                        "This code uses NestJS Prisma to query a relational database",
                    ),
                ),
                (
                    r"PrismaClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS PrismaClient class for direct Prisma ORM database access",
                        "This code uses NestJS Prisma to query a relational database",
                    ),
                ),
                (
                    r"InjectRepository\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS InjectRepository decorator injecting a TypeORM repository",
                        "This code uses NestJS TypeORM to interact with a relational database repository",
                    ),
                ),
                (
                    r"InjectModel\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS InjectModel decorator injecting a Mongoose model",
                        "This code uses NestJS Mongoose to interact with a MongoDB model",
                    ),
                ),
                (
                    r"@nestjs/prisma",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS Prisma module import for Prisma ORM integration",
                        "This code uses NestJS Prisma to interact with a relational database",
                    ),
                ),
                (
                    r"from ['\"]nest-neo4j['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS nest-neo4j import for Neo4j graph database integration",
                        "This code uses NestJS Neo4j to interact with a graph database",
                    ),
                ),
                (
                    r"Neo4jService",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS Neo4jService class providing Neo4j graph database access",
                        "This code uses NestJS Neo4j to interact with a graph database",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"@MessagePattern\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @MessagePattern decorator handling inbound microservice messages",
                        "This code uses NestJS microservices to receive inbound messages",
                    ),
                ),
                (
                    r"@EventPattern\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @EventPattern decorator handling inbound microservice events",
                        "This code uses NestJS microservices to receive inbound events",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"@WebSocketGateway",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @WebSocketGateway decorator defining an inbound WebSocket gateway",
                        "This code uses NestJS to accept inbound WebSocket connections",
                    ),
                ),
                (
                    r"@SubscribeMessage",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @SubscribeMessage decorator handling inbound WebSocket messages",
                        "This code uses NestJS to receive inbound WebSocket messages",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]@nestjs/graphql['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS GraphQL module import for GraphQL API integration",
                        "This code uses NestJS GraphQL to expose an inbound GraphQL API",
                    ),
                ),
                (
                    r"@Query\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Query decorator handling inbound GraphQL query operations",
                        "This code uses NestJS GraphQL to handle inbound GraphQL queries",
                    ),
                ),
                (
                    r"@Mutation\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Mutation decorator handling inbound GraphQL mutation operations",
                        "This code uses NestJS GraphQL to handle inbound GraphQL mutations",
                    ),
                ),
                (
                    r"@Resolver\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Resolver decorator defining a GraphQL resolver class",
                        "This code uses NestJS GraphQL to expose an inbound GraphQL API",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"@nestjs-modules/mailer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS mailer module import for email sending",
                        "This code uses NestJS mailer to send outbound email messages",
                    ),
                ),
                (
                    r"MailerService",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS MailerService class for sending outbound emails",
                        "This code uses NestJS mailer to send outbound email messages",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"@nestjs/cache-manager",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS cache-manager module import for cache integration",
                        "This code uses NestJS cache-manager to interact with a cache",
                    ),
                ),
                (
                    r"CacheModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "NestJS CacheModule class configuring cache integration",
                        "This code uses NestJS to interact with a cache",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"@Sse\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS @Sse decorator exposing an inbound server-sent events endpoint",
                        "This code uses NestJS to expose an inbound SSE streaming endpoint",
                    ),
                ),
                (
                    r"Observable<MessageEvent>",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "NestJS Observable<MessageEvent> stream for server-sent events",
                        "This code uses NestJS to expose an inbound SSE streaming endpoint",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"@Cron\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "NestJS @Cron decorator scheduling a cron-based task",
                        "This code uses NestJS to interact with scheduled cron tasks",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/schedule['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "NestJS schedule module import for task scheduling",
                        "This code uses NestJS to interact with scheduled tasks",
                    ),
                ),
                (
                    r"ScheduleModule",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "NestJS ScheduleModule class configuring task scheduling",
                        "This code uses NestJS to interact with scheduled tasks",
                    ),
                ),
            ],
        },
    },
)
