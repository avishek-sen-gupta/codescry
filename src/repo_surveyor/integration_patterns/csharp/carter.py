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
                        "Carter CarterModule class defining a group of HTTP route handlers",
                        "This code uses Carter to expose inbound HTTP REST route handlers",
                    ),
                ),
                (
                    r"ICarterModule",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Carter ICarterModule interface implemented to register HTTP routes",
                        "This code uses Carter to expose inbound HTTP REST route handlers",
                    ),
                ),
            ],
        },
    },
)
