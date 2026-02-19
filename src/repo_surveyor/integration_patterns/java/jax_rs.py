"""JAX-RS framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="JAX-RS",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@GET\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"@POST\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PUT\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"@DELETE\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Path\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Produces", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Consumes", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
