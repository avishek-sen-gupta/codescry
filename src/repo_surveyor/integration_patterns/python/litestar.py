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
                        "Litestar framework import for building REST API applications",
                        "This code uses Litestar to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"Litestar\(",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Litestar application instantiation for configuring the API server",
                        "This code uses Litestar to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"@get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Litestar @get decorator defining an inbound GET REST endpoint",
                        "This code uses Litestar to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Litestar @post decorator defining an inbound POST REST endpoint",
                        "This code uses Litestar to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Litestar @put decorator defining an inbound PUT REST endpoint",
                        "This code uses Litestar to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Litestar @delete decorator defining an inbound DELETE REST endpoint",
                        "This code uses Litestar to expose an inbound REST API endpoint",
                    ),
                ),
            ],
        },
    },
)
