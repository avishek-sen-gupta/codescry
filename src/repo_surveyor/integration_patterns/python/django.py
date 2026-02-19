"""Django framework integration patterns."""

from ..types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Django",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@api_view", Confidence.HIGH, SignalDirection.INWARD),
                (r"@action", Confidence.MEDIUM, SignalDirection.INWARD),
                (
                    r"from django\.http import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"from rest_framework", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"APIView", Confidence.HIGH, SignalDirection.INWARD),
                (r"ViewSet", Confidence.HIGH, SignalDirection.INWARD),
                (r"GenericAPIView", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"from django\.db import", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"models\.Model", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ForeignKey\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (
                    r"from neomodel import.*StructuredNode",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"neomodel\.db\.cypher_query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"graphene_django", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"django\.core\.mail", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"send_mail", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"django\.core\.cache", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"StreamingHttpResponse", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"django_celery_beat", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
