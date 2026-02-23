"""Actix framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Actix",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"use actix_web::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP server framework is imported",
                        "HTTP server is integrated with Actix",
                    ),
                ),
                (
                    r"#\[get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET route is defined for inbound requests",
                        "GET HTTP requests are handled by Actix",
                    ),
                ),
                (
                    r"#\[post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST route is defined for inbound requests",
                        "POST HTTP requests are handled by Actix",
                    ),
                ),
                (
                    r"#\[put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT route is defined for inbound requests",
                        "PUT HTTP requests are handled by Actix",
                    ),
                ),
                (
                    r"#\[delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE route is defined for inbound requests",
                        "DELETE HTTP requests are handled by Actix",
                    ),
                ),
                (
                    r"#\[patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PATCH route is defined for inbound requests",
                        "PATCH HTTP requests are handled by Actix",
                    ),
                ),
                (
                    r"HttpResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is constructed for inbound handling",
                        "HTTP requests are handled by Actix server",
                    ),
                ),
                (
                    r"HttpRequest",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request is represented for inbound handling",
                        "HTTP requests are handled by Actix server",
                    ),
                ),
                (
                    r"web::Json",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JSON request body is deserialized with extractor",
                        "HTTP JSON data is received by Actix",
                    ),
                ),
                (
                    r"web::Path",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Route parameters are extracted with path extractor",
                        "HTTP route parameters are received by Actix",
                    ),
                ),
                (
                    r"web::Query",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Query parameters are extracted with string extractor",
                        "HTTP query parameters are received by Actix",
                    ),
                ),
                (
                    r"web::Data",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Application state is shared with data extractor",
                        "HTTP application state is shared with Actix",
                    ),
                ),
            ],
        },
    },
)
