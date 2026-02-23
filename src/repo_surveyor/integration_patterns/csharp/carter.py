"""Carter framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Carter",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"CarterModule",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP route handlers are grouped by CarterModule class",
                        "HTTP REST routes are exposed for inbound requests",
                    ),
                ),
                (
                    r"ICarterModule",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP routes are registered via ICarterModule interface",
                        "HTTP REST routes are exposed for inbound requests",
                    ),
                ),
            ],
        },
    },
)
