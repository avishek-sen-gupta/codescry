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
                        "HTTP router integration is imported via go-chi/chi",
                        "HTTP routing layer is accessed",
                    ),
                ),
                (
                    r"chi\.NewRouter\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Chi HTTP router is created by NewRouter function",
                        "HTTP REST routes are exposed for inbound requests",
                    ),
                ),
            ],
        },
    },
)
