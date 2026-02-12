"""ASP.NET Core framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="ASP.NET Core",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"IApplicationBuilder", Confidence.HIGH),
                (r"app\.MapGet", Confidence.HIGH),
                (r"app\.MapPost", Confidence.HIGH),
                (r"WebApplication\.CreateBuilder", Confidence.HIGH),
                (r"MinimalApi", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"MapGrpcService", Confidence.HIGH),
                (r"Grpc\.AspNetCore", Confidence.HIGH),
            ],
        },
    },
)
