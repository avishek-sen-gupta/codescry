"""aiohttp framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "aiohttp"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"from aiohttp import", Confidence.MEDIUM),
        ],
    },
}
