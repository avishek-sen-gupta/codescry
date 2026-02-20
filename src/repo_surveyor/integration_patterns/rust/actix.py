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
                (r"use actix_web::", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"#\[get\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[post\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[put\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[delete\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"#\[patch\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"HttpResponse", Confidence.HIGH, SignalDirection.INWARD),
                (r"HttpRequest", Confidence.HIGH, SignalDirection.INWARD),
                (r"web::Json", Confidence.HIGH, SignalDirection.INWARD),
                (r"web::Path", Confidence.HIGH, SignalDirection.INWARD),
                (r"web::Query", Confidence.HIGH, SignalDirection.INWARD),
                (r"web::Data", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
