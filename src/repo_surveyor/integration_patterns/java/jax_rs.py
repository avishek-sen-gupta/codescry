"""JAX-RS framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="JAX-RS",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@GET\b", Confidence.HIGH),
                (r"@POST\b", Confidence.HIGH),
                (r"@PUT\b", Confidence.HIGH),
                (r"@DELETE\b", Confidence.HIGH),
                (r"@Path\(", Confidence.HIGH),
                (r"@Produces", Confidence.HIGH),
                (r"@Consumes", Confidence.HIGH),
            ],
        },
    },
)
