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
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"AddDbContext", Confidence.HIGH),
                (r"DbContextOptions", Confidence.HIGH),
                (r"UseNpgsql\(", Confidence.HIGH),
                (r"UseSqlServer\(", Confidence.HIGH),
                (r"UseSqlite\(", Confidence.HIGH),
                (r"UseMySql\(", Confidence.HIGH),
                (r"UseInMemoryDatabase\(", Confidence.HIGH),
                (r"AddEntityFrameworkStores", Confidence.HIGH),
                (r"AddNeo4j\(", Confidence.HIGH),
                (r"services\.AddSingleton<IDriver>", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"MapGrpcService", Confidence.HIGH),
                (r"Grpc\.AspNetCore", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"app\.MapGraphQL", Confidence.HIGH),
                (r"AddGraphQLServer", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"AddStackExchangeRedisCache", Confidence.HIGH),
                (r"AddDistributedMemoryCache", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"AddHangfire", Confidence.HIGH),
                (r"UseHangfireDashboard", Confidence.HIGH),
                (r"RecurringJob", Confidence.HIGH),
            ],
        },
    },
)
