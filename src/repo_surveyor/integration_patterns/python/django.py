"""Django framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Django",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@api_view", Confidence.HIGH),
                (r"@action", Confidence.MEDIUM),
                (r"from django\.http import", Confidence.HIGH),
                (r"from rest_framework", Confidence.HIGH),
                (r"APIView", Confidence.HIGH),
                (r"ViewSet", Confidence.HIGH),
                (r"GenericAPIView", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            "patterns": [
                (r"from django\.db import", Confidence.HIGH),
                (r"models\.Model", Confidence.HIGH),
                (r"ForeignKey\(", Confidence.MEDIUM),
            ],
        },
    },
)
