"""ASP.NET Core framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="ASP.NET Core",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"IApplicationBuilder", Confidence.HIGH, SignalDirection.INWARD),
                (r"app\.MapGet", Confidence.HIGH, SignalDirection.INWARD),
                (r"app\.MapPost", Confidence.HIGH, SignalDirection.INWARD),
                (r"WebApplication\.CreateBuilder", Confidence.HIGH, SignalDirection.INWARD),
                (r"MinimalApi", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"AddDbContext", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"DbContextOptions", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"UseNpgsql\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"UseSqlServer\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"UseSqlite\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"UseMySql\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"UseInMemoryDatabase\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"AddEntityFrameworkStores", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"AddNeo4j\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"services\.AddSingleton<IDriver>", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"MapGrpcService", Confidence.HIGH, SignalDirection.INWARD),
                (r"Grpc\.AspNetCore", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"app\.MapGraphQL", Confidence.HIGH, SignalDirection.INWARD),
                (r"AddGraphQLServer", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"AddStackExchangeRedisCache", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"AddDistributedMemoryCache", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"AddHangfire", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"UseHangfireDashboard", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"RecurringJob", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
