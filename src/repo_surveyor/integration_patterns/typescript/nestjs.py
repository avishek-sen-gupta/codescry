"""NestJS framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="NestJS",
    patterns={
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
)
