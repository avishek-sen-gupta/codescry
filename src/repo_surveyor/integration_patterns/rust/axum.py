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
                (
                    r"use axum::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Axum library import for async HTTP server framework",
                        "This code uses Axum to interact with an HTTP server",
                    ),
                ),
                (
                    r"Router::",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Axum Router type defining inbound HTTP route configuration",
                        "This code uses Axum to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"\.route\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Axum .route() method registering an inbound HTTP route handler",
                        "This code uses Axum to handle inbound HTTP requests",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"axum::response::sse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Axum response::sse module exposing an inbound SSE streaming endpoint",
                        "This code uses Axum to expose an inbound server-sent events stream",
                    ),
                ),
            ],
        },
    },
)
