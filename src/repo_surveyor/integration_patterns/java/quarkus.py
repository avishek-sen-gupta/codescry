"""Quarkus framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Quarkus",
    patterns={
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
    },
)
