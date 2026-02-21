"""Nancy framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Nancy",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"NancyModule",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Nancy NancyModule class defining HTTP route handlers for a Nancy application",
                        "This code uses Nancy to expose inbound HTTP REST route handlers",
                    ),
                ),
                (
                    r"Get\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Nancy Get[] route declaration handling incoming HTTP GET requests",
                        "This code uses Nancy to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"Post\[",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Nancy Post[] route declaration handling incoming HTTP POST requests",
                        "This code uses Nancy to handle incoming HTTP POST requests",
                    ),
                ),
            ],
        },
    },
)
