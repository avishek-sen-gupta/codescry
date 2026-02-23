"""Django framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
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
                (
                    r"@api_view",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "API endpoint is decorated for inbound REST handling",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"@action",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "ViewSet action is defined by @action decorator",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"from django\.http import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP requests are handled by module import",
                        "HTTP endpoint is accessed",
                    ),
                ),
                (
                    r"from rest_framework",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "REST framework is imported for API construction",
                        "REST API is accessed via framework",
                    ),
                ),
                (
                    r"APIView",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "REST API view is defined for inbound requests",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"ViewSet",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Resource API endpoint is defined by ViewSet",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
                (
                    r"GenericAPIView",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Generic API view is defined for reusable endpoint logic",
                        "REST endpoint is exposed for inbound API",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"from django\.db import",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is accessed via ORM module import",
                        "Database is accessed via ORM",
                    ),
                ),
                (
                    r"models\.Model",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database model is defined by Model class",
                        "Database model is defined for persistence",
                    ),
                ),
                (
                    r"ForeignKey\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database association is defined by ForeignKey field",
                        "Database is accessed via ORM",
                    ),
                ),
                (
                    r"from neomodel import.*StructuredNode",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database model is defined for Neo4j",
                        "Database model is defined for persistence",
                    ),
                ),
                (
                    r"neomodel\.db\.cypher_query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cypher query is executed against Neo4j database",
                        "Database is queried via neomodel",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"graphene_django",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL schema is exposed via Graphene-Django integration",
                        "GraphQL schema is exposed for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"django\.core\.mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email messages are sent via mail module",
                        "Email message is sent",
                    ),
                ),
                (
                    r"send_mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is dispatched via send_mail utility",
                        "Email message is sent",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"django\.core\.cache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cache store is accessed via core module",
                        "Cache store is connected",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"StreamingHttpResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is streamed for inbound connections",
                        "Server-sent events are streamed inbound",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"django_celery_beat",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Periodic tasks are scheduled by Celery Beat integration",
                        "Scheduled task is handled by Celery",
                    ),
                ),
            ],
        },
    },
)
