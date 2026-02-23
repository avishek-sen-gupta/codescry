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
                        "HTTP controller is annotated for inbound requests",
                        "HTTP request is handled inbound",
                    ),
                ),
                (
                    r"@Get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET request is handled by inbound route",
                        "GET HTTP request is handled inbound",
                    ),
                ),
                (
                    r"@Post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST request is handled by inbound route",
                        "POST HTTP request is handled inbound",
                    ),
                ),
                (
                    r"@Put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT request is handled by inbound route",
                        "PUT HTTP request is handled inbound",
                    ),
                ),
                (
                    r"@Delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE request is handled by inbound route",
                        "DELETE HTTP request is handled inbound",
                    ),
                ),
                (
                    r"@Patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PATCH request is handled by inbound route",
                        "PATCH HTTP request is handled inbound",
                    ),
                ),
                (
                    r"@Body\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Request body is extracted from inbound request",
                        "HTTP request body is received inbound",
                    ),
                ),
                (
                    r"@Param\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Route parameters are extracted from inbound request",
                        "HTTP route parameters are received inbound",
                    ),
                ),
                (
                    r"@Query\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Query parameters are extracted from inbound request",
                        "HTTP query parameters are received inbound",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/common['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Core framework components are imported",
                        "Framework integration is implemented",
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
                        "Relational database integration is imported via TypeORM",
                        "Database is accessed with TypeORM",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/mongoose['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB integration is imported via Mongoose",
                        "MongoDB database is accessed",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/sequelize['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Relational database integration is imported via Sequelize",
                        "Database is accessed with Sequelize",
                    ),
                ),
                (
                    r"TypeOrmModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeORM database integration is configured",
                        "Database is accessed with TypeORM",
                    ),
                ),
                (
                    r"MongooseModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Mongoose MongoDB integration is configured",
                        "MongoDB database is accessed",
                    ),
                ),
                (
                    r"SequelizeModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Sequelize database integration is configured",
                        "Database is accessed with Sequelize",
                    ),
                ),
                (
                    r"PrismaService",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Prisma database is accessed by service",
                        "Database is queried with Prisma",
                    ),
                ),
                (
                    r"PrismaClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Prisma database is accessed directly",
                        "Database is queried with Prisma",
                    ),
                ),
                (
                    r"InjectRepository\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "TypeORM repository is injected with decorator",
                        "Database repository is accessed with TypeORM",
                    ),
                ),
                (
                    r"InjectModel\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Mongoose model is injected with decorator",
                        "MongoDB model is accessed",
                    ),
                ),
                (
                    r"@nestjs/prisma",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Prisma ORM integration is imported",
                        "Database is accessed with Prisma",
                    ),
                ),
                (
                    r"from ['\"]nest-neo4j['\"]",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j database integration is imported",
                        "Graph database is accessed",
                    ),
                ),
                (
                    r"Neo4jService",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j database is accessed by service",
                        "Graph database is accessed",
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
                        "Microservice messages are handled by inbound pattern",
                        "Messages are received inbound",
                    ),
                ),
                (
                    r"@EventPattern\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Microservice events are handled by inbound pattern",
                        "Events are received inbound",
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
                        "WebSocket gateway is defined for inbound connections",
                        "WebSocket connections are accepted inbound",
                    ),
                ),
                (
                    r"@SubscribeMessage",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket messages are handled by inbound subscription",
                        "WebSocket messages are received inbound",
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
                        "GraphQL API integration is imported",
                        "GraphQL API is exposed for inbound requests",
                    ),
                ),
                (
                    r"@Query\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL query is handled by inbound operation",
                        "GraphQL queries are handled inbound",
                    ),
                ),
                (
                    r"@Mutation\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL mutation is handled by inbound operation",
                        "GraphQL mutations are handled inbound",
                    ),
                ),
                (
                    r"@Resolver\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL resolver is defined with decorator",
                        "GraphQL API is exposed for inbound requests",
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
                        "Email sending is imported via mailer",
                        "Email messages are sent outbound",
                    ),
                ),
                (
                    r"MailerService",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Outbound emails are sent by MailerService",
                        "Email messages are sent outbound",
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
                        "Cache integration is imported via cache-manager",
                        "Cache is accessed",
                    ),
                ),
                (
                    r"CacheModule",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache integration is configured with CacheModule",
                        "Cache is accessed",
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
                        "Server-sent events endpoint is exposed for inbound",
                        "SSE streaming endpoint is exposed inbound",
                    ),
                ),
                (
                    r"Observable<MessageEvent>",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-sent events are streamed via Observable",
                        "SSE streaming endpoint is exposed inbound",
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
                        "Cron task is scheduled with decorator",
                        "Cron tasks are scheduled",
                    ),
                ),
                (
                    r"from ['\"]@nestjs/schedule['\"]",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Task scheduling is imported",
                        "Tasks are scheduled",
                    ),
                ),
                (
                    r"ScheduleModule",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Task scheduling is configured with ScheduleModule",
                        "Tasks are scheduled",
                    ),
                ),
            ],
        },
    },
)
