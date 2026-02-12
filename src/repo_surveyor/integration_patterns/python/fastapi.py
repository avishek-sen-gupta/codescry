"""FastAPI framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "FastAPI"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"@\w+\.get", Confidence.HIGH),
            (r"@\w+\.post", Confidence.HIGH),
            (r"@\w+\.put", Confidence.HIGH),
            (r"@\w+\.delete", Confidence.HIGH),
            (r"from fastapi import", Confidence.HIGH),
        ],
    },
}
