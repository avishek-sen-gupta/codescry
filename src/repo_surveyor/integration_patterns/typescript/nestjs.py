"""NestJS framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="NestJS",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
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
            PatternKey.PATTERNS: [
                (r"@MessagePattern\(", Confidence.HIGH),
                (r"@EventPattern\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"@WebSocketGateway", Confidence.HIGH),
                (r"@SubscribeMessage", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"from ['\"]@nestjs/graphql['\"]", Confidence.HIGH),
                (r"@Query\(\)", Confidence.HIGH),
                (r"@Mutation\(\)", Confidence.HIGH),
                (r"@Resolver\(\)", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"@nestjs-modules/mailer", Confidence.HIGH),
                (r"MailerService", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"@nestjs/cache-manager", Confidence.HIGH),
                (r"CacheModule", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"@Sse\(\)", Confidence.HIGH),
                (r"Observable<MessageEvent>", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"@Cron\(", Confidence.HIGH),
                (r"from ['\"]@nestjs/schedule['\"]", Confidence.HIGH),
                (r"ScheduleModule", Confidence.HIGH),
            ],
        },
    },
)
