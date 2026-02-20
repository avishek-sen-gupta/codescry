"""Dropwizard framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Dropwizard",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.dropwizard", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Path\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Produces", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Consumes", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
