"""Gorilla framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Gorilla",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/gorilla/mux"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Gorilla gorilla/mux package import for the Gorilla HTTP router",
                        "This code uses Gorilla to interact with an HTTP routing layer",
                    ),
                ),
                (
                    r"mux\.NewRouter\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Gorilla mux.NewRouter function creating a new Gorilla Mux HTTP router",
                        "This code uses Gorilla to expose inbound HTTP REST routes",
                    ),
                ),
            ],
        },
    },
)
