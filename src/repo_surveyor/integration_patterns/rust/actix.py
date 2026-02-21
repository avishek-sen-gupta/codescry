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
                        "Actix actix-web library import for HTTP server framework",
                        "This code uses Actix to interact with an HTTP server",
                    ),
                ),
                (
                    r"#\[get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix #[get] macro defining an inbound GET HTTP route handler",
                        "This code uses Actix to handle inbound GET HTTP requests",
                    ),
                ),
                (
                    r"#\[post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix #[post] macro defining an inbound POST HTTP route handler",
                        "This code uses Actix to handle inbound POST HTTP requests",
                    ),
                ),
                (
                    r"#\[put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix #[put] macro defining an inbound PUT HTTP route handler",
                        "This code uses Actix to handle inbound PUT HTTP requests",
                    ),
                ),
                (
                    r"#\[delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix #[delete] macro defining an inbound DELETE HTTP route handler",
                        "This code uses Actix to handle inbound DELETE HTTP requests",
                    ),
                ),
                (
                    r"#\[patch\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix #[patch] macro defining an inbound PATCH HTTP route handler",
                        "This code uses Actix to handle inbound PATCH HTTP requests",
                    ),
                ),
                (
                    r"HttpResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix HttpResponse type constructing an inbound HTTP response",
                        "This code uses Actix to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"HttpRequest",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix HttpRequest type representing an inbound HTTP request",
                        "This code uses Actix to handle inbound HTTP requests",
                    ),
                ),
                (
                    r"web::Json",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix web::Json extractor deserializing inbound JSON request body",
                        "This code uses Actix to receive inbound HTTP JSON data",
                    ),
                ),
                (
                    r"web::Path",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix web::Path extractor extracting inbound route path parameters",
                        "This code uses Actix to receive inbound HTTP route parameters",
                    ),
                ),
                (
                    r"web::Query",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Actix web::Query extractor extracting inbound query string parameters",
                        "This code uses Actix to receive inbound HTTP query parameters",
                    ),
                ),
                (
                    r"web::Data",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Actix web::Data extractor sharing application state in HTTP handlers",
                        "This code uses Actix to interact with shared HTTP application state",
                    ),
                ),
            ],
        },
    },
)
