"""Dropwizard framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Dropwizard",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"import io\.dropwizard", Confidence.HIGH),
                (r"@Path\(", Confidence.HIGH),
                (r"@Produces", Confidence.HIGH),
                (r"@Consumes", Confidence.HIGH),
            ],
        },
    },
)
