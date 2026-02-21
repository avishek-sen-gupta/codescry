"""Chi framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Chi",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/go-chi/chi"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Chi go-chi/chi package import for lightweight HTTP router integration",
                        "This code uses Chi to interact with an HTTP routing layer",
                    ),
                ),
                (
                    r"chi\.NewRouter\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Chi chi.NewRouter function creating a new Chi HTTP router",
                        "This code uses Chi to expose inbound HTTP REST routes",
                    ),
                ),
            ],
        },
    },
)
