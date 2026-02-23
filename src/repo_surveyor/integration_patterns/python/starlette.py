"""Starlette framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Starlette",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from starlette",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "ASGI web applications are built with framework",
                        "HTTP endpoint is accessed",
                    ),
                ),
            ],
        },
    },
)
