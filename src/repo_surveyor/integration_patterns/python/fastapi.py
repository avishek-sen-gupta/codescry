"""FastAPI framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="FastAPI",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"@\w+\.get",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "FastAPI @app.get decorator defining an inbound GET REST endpoint",
                        "This code uses FastAPI to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@\w+\.post",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "FastAPI @app.post decorator defining an inbound POST REST endpoint",
                        "This code uses FastAPI to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@\w+\.put",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "FastAPI @app.put decorator defining an inbound PUT REST endpoint",
                        "This code uses FastAPI to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@\w+\.delete",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "FastAPI @app.delete decorator defining an inbound DELETE REST endpoint",
                        "This code uses FastAPI to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"from fastapi import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "FastAPI framework import for building REST API applications",
                        "This code uses FastAPI to interact with an HTTP endpoint",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"from sqlmodel import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FastAPI SQLModel ORM import for database model and query operations",
                        "This code uses FastAPI SQLModel to interact with a relational database",
                    ),
                ),
                (
                    r"import sqlmodel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FastAPI SQLModel library import for database access",
                        "This code uses FastAPI SQLModel to interact with a relational database",
                    ),
                ),
                (
                    r"SQLModel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FastAPI SQLModel class for combined Pydantic and ORM database model definition",
                        "This code uses FastAPI SQLModel to define a database model for persistence",
                    ),
                ),
                (
                    r"from databases import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FastAPI databases library import for async database access",
                        "This code uses FastAPI databases to query a relational database",
                    ),
                ),
                (
                    r"import databases",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FastAPI databases library for async database query execution",
                        "This code uses FastAPI databases to query a relational database",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"from strawberry\.fastapi",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "FastAPI Strawberry GraphQL integration for inbound GraphQL endpoint",
                        "This code uses FastAPI Strawberry to expose an inbound GraphQL schema",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"EventSourceResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "FastAPI EventSourceResponse for server-sent event stream delivery",
                        "This code uses FastAPI to expose an inbound server-sent event stream",
                    ),
                ),
                (
                    r"StreamingResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "FastAPI StreamingResponse for chunked HTTP response streaming",
                        "This code uses FastAPI to expose an inbound server-sent event stream",
                    ),
                ),
            ],
        },
    },
)
