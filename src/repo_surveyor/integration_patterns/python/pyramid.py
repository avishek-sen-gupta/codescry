"""Pyramid framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Pyramid"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"from pyramid import", Confidence.HIGH),
            (r"from pyramid\.view import", Confidence.HIGH),
            (r"@view_config", Confidence.HIGH),
            (r"from pyramid\.config import", Confidence.HIGH),
        ],
    },
}
