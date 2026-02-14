"""FastAPI framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="FastAPI",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@\w+\.get", Confidence.HIGH),
                (r"@\w+\.post", Confidence.HIGH),
                (r"@\w+\.put", Confidence.HIGH),
                (r"@\w+\.delete", Confidence.HIGH),
                (r"from fastapi import", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"from sqlmodel import", Confidence.HIGH),
                (r"import sqlmodel", Confidence.HIGH),
                (r"SQLModel", Confidence.HIGH),
                (r"from databases import", Confidence.HIGH),
                (r"import databases", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"from strawberry\.fastapi", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"EventSourceResponse", Confidence.HIGH),
                (r"StreamingResponse", Confidence.HIGH),
            ],
        },
    },
)
