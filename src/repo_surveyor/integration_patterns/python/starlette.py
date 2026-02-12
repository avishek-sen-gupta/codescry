"""Starlette framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Starlette"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"from starlette", Confidence.MEDIUM),
        ],
    },
}
