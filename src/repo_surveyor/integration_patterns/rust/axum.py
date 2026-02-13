"""Axum framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Axum",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"use axum::", Confidence.HIGH),
                (r"Router::", Confidence.MEDIUM),
                (r"\.route\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"axum::response::sse", Confidence.HIGH),
            ],
        },
    },
)
