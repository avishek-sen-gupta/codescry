"""Axum framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Axum"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"use axum::", Confidence.HIGH),
            (r"Router::", Confidence.MEDIUM),
            (r"\.route\(", Confidence.MEDIUM),
        ],
    },
}
