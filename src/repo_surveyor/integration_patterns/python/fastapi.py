"""FastAPI framework integration patterns."""

from ..types import (
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
                (r"@\w+\.get", Confidence.HIGH, SignalDirection.INWARD),
                (r"@\w+\.post", Confidence.HIGH, SignalDirection.INWARD),
                (r"@\w+\.put", Confidence.HIGH, SignalDirection.INWARD),
                (r"@\w+\.delete", Confidence.HIGH, SignalDirection.INWARD),
                (r"from fastapi import", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"from sqlmodel import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import sqlmodel", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SQLModel", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"from databases import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import databases", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"from strawberry\.fastapi", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"EventSourceResponse", Confidence.HIGH, SignalDirection.INWARD),
                (r"StreamingResponse", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
