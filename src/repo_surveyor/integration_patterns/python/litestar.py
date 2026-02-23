"""Litestar framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Litestar",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from litestar import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Litestar framework is imported for REST API",
                        "HTTP endpoint is accessed",
                    ),
                ),
                (
                    r"Litestar\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "API server is created with Litestar configuration",
                        "HTTP endpoint is accessed",
                    ),
                ),
                (
                    r"@get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET endpoint is defined with decorator",
                        "REST API endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"@post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST endpoint is defined with decorator",
                        "REST API endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"@put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT endpoint is defined with decorator",
                        "REST API endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"@delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE endpoint is defined with decorator",
                        "REST API endpoint is exposed for inbound requests",
                    ),
                ),
            ],
        },
    },
)
