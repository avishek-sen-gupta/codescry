"""ASP.NET Core framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "ASP.NET Core"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"IApplicationBuilder", Confidence.HIGH),
            (r"app\.MapGet", Confidence.HIGH),
            (r"app\.MapPost", Confidence.HIGH),
            (r"WebApplication\.CreateBuilder", Confidence.HIGH),
            (r"MinimalApi", Confidence.HIGH),
        ],
    },
    IntegrationType.GRPC: {
        "patterns": [
            (r"MapGrpcService", Confidence.HIGH),
            (r"Grpc\.AspNetCore", Confidence.HIGH),
        ],
    },
}
