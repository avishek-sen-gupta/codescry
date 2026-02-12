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
    },
)
