"""Dropwizard framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Dropwizard",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.dropwizard", Confidence.HIGH),
                (r"@Path\(", Confidence.HIGH),
                (r"@Produces", Confidence.HIGH),
                (r"@Consumes", Confidence.HIGH),
            ],
        },
    },
)
