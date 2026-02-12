"""Actix framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Actix"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"use actix_web::", Confidence.HIGH),
            (r"#\[get\(", Confidence.HIGH),
            (r"#\[post\(", Confidence.HIGH),
            (r"#\[put\(", Confidence.HIGH),
            (r"#\[delete\(", Confidence.HIGH),
            (r"#\[patch\(", Confidence.HIGH),
            (r"HttpResponse", Confidence.HIGH),
            (r"HttpRequest", Confidence.HIGH),
            (r"web::Json", Confidence.HIGH),
            (r"web::Path", Confidence.HIGH),
            (r"web::Query", Confidence.HIGH),
            (r"web::Data", Confidence.HIGH),
        ],
    },
}
