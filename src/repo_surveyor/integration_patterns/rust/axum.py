"""Axum framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Axum",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"use axum::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Router::", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\.route\(", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"axum::response::sse", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
