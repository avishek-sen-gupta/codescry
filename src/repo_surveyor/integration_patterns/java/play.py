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
                        "Play import play.mvc for building Play Framework Java MVC controllers",
                        "This code uses Play to expose an inbound HTTP endpoint",
                    ),
                ),
                (
                    r"import play\.api\.mvc",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play import play.api.mvc for building Play Framework Scala MVC controllers",
                        "This code uses Play to expose an inbound HTTP endpoint",
                    ),
                ),
                (
                    r"Action \{",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play Action block defining a synchronous HTTP request handler",
                        "This code uses Play to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"Action\.async",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Play Action.async for defining an asynchronous HTTP request handler",
                        "This code uses Play to handle inbound HTTP requests asynchronously",
                    ),
                ),
            ],
        },
    },
)
