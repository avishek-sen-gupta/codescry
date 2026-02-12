"""Warp framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Warp"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"use warp::", Confidence.HIGH),
        ],
    },
}
