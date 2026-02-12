"""Dropwizard framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Dropwizard"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"import io\.dropwizard", Confidence.HIGH),
            (r"@Path\(", Confidence.HIGH),
            (r"@Produces", Confidence.HIGH),
            (r"@Consumes", Confidence.HIGH),
        ],
    },
}
