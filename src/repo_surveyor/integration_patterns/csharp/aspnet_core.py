"""ASP.NET Core framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="ASP.NET Core",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"IApplicationBuilder",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP pipeline is configured with middleware",
                        "HTTP middleware pipeline is exposed for inbound requests",
                    ),
                ),
                (
                    r"app\.MapGet",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET endpoint is registered with minimal API",
                        "HTTP GET requests are handled for incoming traffic",
                    ),
                ),
                (
                    r"app\.MapPost",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST endpoint is registered with minimal API",
                        "HTTP POST requests are handled for incoming traffic",
                    ),
                ),
                (
                    r"WebApplication\.CreateBuilder",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Web host is initialized with builder factory",
                        "Web application host is exposed for inbound requests",
                    ),
                ),
                (
                    r"MinimalApi",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP endpoints are defined with minimal pattern",
                        "Minimal API endpoints are exposed for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"AddDbContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "DbContext is registered with Entity Framework",
                        "Database connection is opened via Entity Framework",
                    ),
                ),
                (
                    r"DbContextOptions",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database context is configured with Entity Framework",
                        "Database connection is configured with Entity Framework",
                    ),
                ),
                (
                    r"UseNpgsql\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database provider is configured with PostgreSQL",
                        "PostgreSQL database is connected via Entity Framework",
                    ),
                ),
                (
                    r"UseSqlServer\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database provider is configured with SQL Server",
                        "SQL Server database is connected via Entity Framework",
                    ),
                ),
                (
                    r"UseSqlite\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database provider is configured with SQLite",
                        "SQLite database is connected via Entity Framework",
                    ),
                ),
                (
                    r"UseMySql\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database provider is configured with MySQL",
                        "MySQL database is connected via Entity Framework",
                    ),
                ),
                (
                    r"UseInMemoryDatabase\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database provider is configured with memory storage",
                        "In-memory database is connected via Entity Framework",
                    ),
                ),
                (
                    r"AddEntityFrameworkStores",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Identity stores are registered with Entity Framework",
                        "Entity Framework database is written by Identity",
                    ),
                ),
                (
                    r"AddNeo4j\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j driver is registered for graph database",
                        "Neo4j graph database is connected for outbound access",
                    ),
                ),
                (
                    r"services\.AddSingleton<IDriver>",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j driver is registered as singleton",
                        "Neo4j graph database is connected for outbound access",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"MapGrpcService",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC service is registered with endpoint",
                        "gRPC service is exposed for inbound requests",
                    ),
                ),
                (
                    r"Grpc\.AspNetCore",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC server is hosted with AspNetCore",
                        "gRPC server is exposed for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"app\.MapGraphQL",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL endpoint is registered with mapping",
                        "GraphQL API is exposed for inbound requests",
                    ),
                ),
                (
                    r"AddGraphQLServer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL server is registered with HotChocolate",
                        "GraphQL server is exposed for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"AddStackExchangeRedisCache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is registered for distributed storage",
                        "Redis cache is written for outbound storage",
                    ),
                ),
                (
                    r"AddDistributedMemoryCache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memory cache is registered for distributed storage",
                        "In-memory distributed cache is written for storage",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"AddHangfire",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background job services are registered with Hangfire",
                        "Background job scheduler is integrated with Hangfire",
                    ),
                ),
                (
                    r"UseHangfireDashboard",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Hangfire dashboard is enabled for monitoring",
                        "Job scheduling dashboard is integrated with Hangfire",
                    ),
                ),
                (
                    r"RecurringJob",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background jobs are scheduled with recurring pattern",
                        "Recurring job scheduler is integrated with Hangfire",
                    ),
                ),
            ],
        },
    },
)
