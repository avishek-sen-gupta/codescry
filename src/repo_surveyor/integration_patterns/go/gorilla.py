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
                        "HTTP router is imported via mux package",
                        "HTTP routing layer is accessed",
                    ),
                ),
                (
                    r"mux\.NewRouter\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP router is created for request routing",
                        "HTTP REST routes are exposed inbound",
                    ),
                ),
            ],
        },
    },
)
