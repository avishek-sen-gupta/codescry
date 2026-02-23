"""Helidon framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Helidon",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import io\.helidon",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported for service building",
                        "HTTP endpoint is exposed inbound",
                    ),
                ),
                (
                    r"WebServer\.builder",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is built and configured",
                        "REST API is exposed inbound",
                    ),
                ),
                (
                    r"HttpRules",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route handlers are registered via rules",
                        "HTTP request is handled inbound",
                    ),
                ),
            ],
        },
    },
)
