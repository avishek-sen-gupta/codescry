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
                        "GET endpoint is decorated for inbound REST",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"@\w+\.post",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST endpoint is decorated for inbound REST",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"@\w+\.put",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT endpoint is decorated for inbound REST",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"@\w+\.delete",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE endpoint is decorated for inbound REST",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"from fastapi import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "REST API framework is imported",
                        "HTTP endpoint is accessed",
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
                        "Database model is imported for ORM operations",
                        "Database is accessed via SQLModel",
                    ),
                ),
                (
                    r"import sqlmodel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database access is imported via SQLModel",
                        "Database is accessed via SQLModel",
                    ),
                ),
                (
                    r"SQLModel",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database model is defined with Pydantic validation",
                        "Database model is defined for persistence",
                    ),
                ),
                (
                    r"from databases import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Async database access is imported",
                        "Database is queried via FastAPI databases",
                    ),
                ),
                (
                    r"import databases",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database queries are executed asynchronously",
                        "Database is queried via FastAPI databases",
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
                        "GraphQL endpoint is integrated for inbound requests",
                        "GraphQL schema is exposed via Strawberry",
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
                        "Event stream is delivered via EventSource response",
                        "Server-sent events are streamed inbound",
                    ),
                ),
                (
                    r"StreamingResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is streamed in chunks",
                        "Server-sent events are streamed inbound",
                    ),
                ),
            ],
        },
    },
)
