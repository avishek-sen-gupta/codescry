"""Quarkus framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Quarkus"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"import io\.quarkus", Confidence.HIGH),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"@Incoming\(", Confidence.HIGH),
            (r"@Outgoing\(", Confidence.HIGH),
        ],
    },
}
