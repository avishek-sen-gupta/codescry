"""Flask framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Flask"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"@\w+\.route", Confidence.HIGH),
            (r"from flask import", Confidence.HIGH),
        ],
    },
}
