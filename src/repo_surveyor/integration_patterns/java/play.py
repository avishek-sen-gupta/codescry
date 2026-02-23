"""Play framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Play",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"import play\.mvc",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "MVC controllers are imported for Java framework",
                        "HTTP endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"import play\.api\.mvc",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "MVC controllers are imported for Scala framework",
                        "HTTP endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"Action \{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request is handled by synchronous action",
                        "HTTP request is handled by controller",
                    ),
                ),
                (
                    r"Action\.async",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request is handled by asynchronous action",
                        "HTTP request is handled asynchronously",
                    ),
                ),
            ],
        },
    },
)
