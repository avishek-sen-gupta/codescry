"""Django framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Django",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
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
            PatternKey.PATTERNS: [
                (r"from django\.db import", Confidence.HIGH),
                (r"models\.Model", Confidence.HIGH),
                (r"ForeignKey\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"graphene_django", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"django\.core\.mail", Confidence.HIGH),
                (r"send_mail", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"django\.core\.cache", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"StreamingHttpResponse", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"django_celery_beat", Confidence.HIGH),
            ],
        },
    },
)
