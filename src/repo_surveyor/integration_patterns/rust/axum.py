"""Axum framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Axum",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"use axum::", Confidence.HIGH),
                (r"Router::", Confidence.MEDIUM),
                (r"\.route\(", Confidence.MEDIUM),
            ],
        },
    },
)
