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
                        "Async HTTP framework is imported",
                        "HTTP server is accessed",
                    ),
                ),
                (
                    r"Router::",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP router is configured for inbound routes",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"\.route\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP route is registered with handler method",
                        "HTTP request is handled for inbound processing",
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
                        "SSE endpoint is exposed for streaming",
                        "SSE stream is exposed for inbound connections",
                    ),
                ),
            ],
        },
    },
)
