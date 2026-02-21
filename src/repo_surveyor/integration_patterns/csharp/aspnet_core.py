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
                        "ASP.NET Core IApplicationBuilder interface configuring the HTTP request pipeline",
                        "This code uses ASP.NET Core to expose an inbound HTTP middleware pipeline",
                    ),
                ),
                (
                    r"app\.MapGet",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ASP.NET Core app.MapGet method registering a minimal API GET endpoint",
                        "This code uses ASP.NET Core to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"app\.MapPost",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ASP.NET Core app.MapPost method registering a minimal API POST endpoint",
                        "This code uses ASP.NET Core to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"WebApplication\.CreateBuilder",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ASP.NET Core WebApplication.CreateBuilder factory initializing a web host",
                        "This code uses ASP.NET Core to expose an inbound web application host",
                    ),
                ),
                (
                    r"MinimalApi",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ASP.NET Core Minimal API pattern defining lightweight HTTP endpoints",
                        "This code uses ASP.NET Core to expose inbound minimal API endpoints",
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
                        "ASP.NET Core AddDbContext method registering an Entity Framework DbContext",
                        "This code uses ASP.NET Core to connect to an outbound database via Entity Framework",
                    ),
                ),
                (
                    r"DbContextOptions",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core DbContextOptions class configuring an Entity Framework database context",
                        "This code uses ASP.NET Core Entity Framework to configure a database connection",
                    ),
                ),
                (
                    r"UseNpgsql\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core UseNpgsql method configuring a PostgreSQL database provider",
                        "This code uses ASP.NET Core Entity Framework to connect to a PostgreSQL database",
                    ),
                ),
                (
                    r"UseSqlServer\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core UseSqlServer method configuring a SQL Server database provider",
                        "This code uses ASP.NET Core Entity Framework to connect to a SQL Server database",
                    ),
                ),
                (
                    r"UseSqlite\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core UseSqlite method configuring a SQLite database provider",
                        "This code uses ASP.NET Core Entity Framework to connect to a SQLite database",
                    ),
                ),
                (
                    r"UseMySql\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core UseMySql method configuring a MySQL database provider",
                        "This code uses ASP.NET Core Entity Framework to connect to a MySQL database",
                    ),
                ),
                (
                    r"UseInMemoryDatabase\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core UseInMemoryDatabase method configuring an in-memory database provider",
                        "This code uses ASP.NET Core Entity Framework to connect to an in-memory database",
                    ),
                ),
                (
                    r"AddEntityFrameworkStores",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core AddEntityFrameworkStores method registering EF-backed identity stores",
                        "This code uses ASP.NET Core Identity to write to an Entity Framework database store",
                    ),
                ),
                (
                    r"AddNeo4j\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core AddNeo4j method registering a Neo4j graph database driver",
                        "This code uses ASP.NET Core to connect to an outbound Neo4j graph database",
                    ),
                ),
                (
                    r"services\.AddSingleton<IDriver>",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core services.AddSingleton<IDriver> registering a Neo4j driver singleton",
                        "This code uses ASP.NET Core to connect to an outbound Neo4j graph database",
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
                        "ASP.NET Core MapGrpcService method registering a gRPC service endpoint",
                        "This code uses ASP.NET Core to expose an inbound gRPC service",
                    ),
                ),
                (
                    r"Grpc\.AspNetCore",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ASP.NET Core Grpc.AspNetCore package hosting a gRPC server",
                        "This code uses ASP.NET Core to expose an inbound gRPC server",
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
                        "ASP.NET Core app.MapGraphQL method registering a GraphQL endpoint",
                        "This code uses ASP.NET Core to expose an inbound GraphQL API",
                    ),
                ),
                (
                    r"AddGraphQLServer",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "ASP.NET Core AddGraphQLServer method registering a HotChocolate GraphQL server",
                        "This code uses ASP.NET Core to expose an inbound GraphQL server",
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
                        "ASP.NET Core AddStackExchangeRedisCache method registering a Redis distributed cache",
                        "This code uses ASP.NET Core to write to an outbound Redis cache",
                    ),
                ),
                (
                    r"AddDistributedMemoryCache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ASP.NET Core AddDistributedMemoryCache method registering an in-memory distributed cache",
                        "This code uses ASP.NET Core to write to an in-memory distributed cache",
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
                        "ASP.NET Core AddHangfire method registering Hangfire background job services",
                        "This code uses ASP.NET Core Hangfire to integrate with a background job scheduler",
                    ),
                ),
                (
                    r"UseHangfireDashboard",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "ASP.NET Core UseHangfireDashboard method enabling the Hangfire monitoring dashboard",
                        "This code uses ASP.NET Core Hangfire to interact with a job scheduling dashboard",
                    ),
                ),
                (
                    r"RecurringJob",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "ASP.NET Core Hangfire RecurringJob class scheduling periodic background jobs",
                        "This code uses ASP.NET Core Hangfire to integrate with a recurring job scheduler",
                    ),
                ),
            ],
        },
    },
)
