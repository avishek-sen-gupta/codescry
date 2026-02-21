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
                        "Django REST Framework @api_view decorator defining an API endpoint",
                        "This code uses Django REST Framework to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"@action",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Django REST Framework @action decorator defining a custom ViewSet action",
                        "This code uses Django REST Framework to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"from django\.http import",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Django HTTP module import for request and response handling",
                        "This code uses Django to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r"from rest_framework",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Django REST Framework import for REST API construction",
                        "This code uses Django REST Framework to interact with a REST API",
                    ),
                ),
                (
                    r"APIView",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Django REST Framework APIView class defining an inbound REST API view",
                        "This code uses Django REST Framework to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"ViewSet",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Django REST Framework ViewSet class defining a resource API endpoint",
                        "This code uses Django REST Framework to expose an inbound REST API endpoint",
                    ),
                ),
                (
                    r"GenericAPIView",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Django REST Framework GenericAPIView class for reusable API endpoint logic",
                        "This code uses Django REST Framework to expose an inbound REST API endpoint",
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
                        "Django database module import for ORM and database access",
                        "This code uses Django ORM to interact with a relational database",
                    ),
                ),
                (
                    r"models\.Model",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Django models.Model defining a database model",
                        "This code uses Django ORM to define a database model for persistence",
                    ),
                ),
                (
                    r"ForeignKey\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Django ForeignKey field defining a relational database association",
                        "This code uses Django ORM to interact with a relational database",
                    ),
                ),
                (
                    r"from neomodel import.*StructuredNode",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Django neomodel StructuredNode for Neo4j graph database model definition",
                        "This code uses Django neomodel to define a database model for persistence",
                    ),
                ),
                (
                    r"neomodel\.db\.cypher_query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Django neomodel Cypher query execution against Neo4j graph database",
                        "This code uses Django neomodel to query a database",
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
                        "Django Graphene-Django integration for GraphQL schema exposure",
                        "This code uses Django to expose an inbound GraphQL schema",
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
                        "Django core mail module for sending email messages",
                        "This code uses Django to send an email message",
                    ),
                ),
                (
                    r"send_mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Django send_mail utility for dispatching outbound email",
                        "This code uses Django to send an email message",
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
                        "Django core cache module for cache store access",
                        "This code uses Django to connect to a cache store",
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
                        "Django StreamingHttpResponse for streaming inbound HTTP responses",
                        "This code uses Django to expose an inbound server-sent event stream",
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
                        "Django Celery Beat integration for periodic task scheduling",
                        "This code uses Django Celery to interact with a scheduled task",
                    ),
                ),
            ],
        },
    },
)
