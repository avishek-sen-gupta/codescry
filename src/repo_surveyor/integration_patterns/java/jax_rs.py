"""JAX-RS framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "JAX-RS"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"@GET\b", Confidence.HIGH),
            (r"@POST\b", Confidence.HIGH),
            (r"@PUT\b", Confidence.HIGH),
            (r"@DELETE\b", Confidence.HIGH),
            (r"@Path\(", Confidence.HIGH),
            (r"@Produces", Confidence.HIGH),
            (r"@Consumes", Confidence.HIGH),
        ],
    },
}
