"""Warp framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Warp",
    import_patterns=(r"use warp::",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"use warp::",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "HTTP server is imported via Warp",
                        "HTTP framework is accessed via Warp",
                    ),
                ),
                (
                    r"warp::serve",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is started with Warp",
                        "HTTP server is bound to address for inbound requests",
                    ),
                ),
                (
                    r"warp::path!",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Route path is defined with macro",
                        "HTTP route is registered for inbound requests",
                    ),
                ),
                (
                    r"warp::path\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Route path is defined for endpoint",
                        "HTTP route is registered for inbound requests",
                    ),
                ),
                (
                    r"warp::get\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET method filter is created",
                        "GET HTTP requests are handled for inbound requests",
                    ),
                ),
                (
                    r"warp::post\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST method filter is created",
                        "POST HTTP requests are handled for inbound requests",
                    ),
                ),
                (
                    r"warp::put\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT method filter is created",
                        "PUT HTTP requests are handled for inbound requests",
                    ),
                ),
                (
                    r"warp::delete\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE method filter is created",
                        "DELETE HTTP requests are handled for inbound requests",
                    ),
                ),
                (
                    r"warp::patch\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PATCH method filter is created",
                        "PATCH HTTP requests are handled for inbound requests",
                    ),
                ),
                (
                    r"warp::head\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HEAD method filter is created",
                        "HEAD HTTP requests are handled for inbound requests",
                    ),
                ),
                (
                    r"\.and\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Filters are composed for route handling",
                        "HTTP route filter is composed for inbound requests",
                    ),
                ),
                (
                    r"\.or\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Routes are combined with alternatives",
                        "HTTP routes are combined for inbound requests",
                    ),
                ),
                (
                    r"\.and_then\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Async handler is chained to route",
                        "HTTP request is handled by async handler",
                    ),
                ),
                (
                    r"warp::body::json\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JSON request body is deserialized from filter",
                        "HTTP JSON data is received for inbound requests",
                    ),
                ),
                (
                    r"warp::body::content_length_limit\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Request body size is limited by filter",
                        "HTTP request body is constrained for inbound requests",
                    ),
                ),
                (
                    r"warp::query::",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Query parameters are extracted from request",
                        "HTTP query parameters are received for inbound requests",
                    ),
                ),
                (
                    r"warp::reply::json",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JSON response is constructed for reply",
                        "HTTP JSON response is returned for inbound requests",
                    ),
                ),
                (
                    r"warp::reply::with_status",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is constructed with status code",
                        "HTTP response status is set for inbound requests",
                    ),
                ),
                (
                    r"warp::log\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Request logging middleware is applied",
                        "HTTP request logging is enabled for server",
                    ),
                ),
                (
                    r"warp::any\(\)",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Catch-all filter is created for requests",
                        "HTTP requests are matched by catch-all filter",
                    ),
                ),
                (
                    r"warp::Filter",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP filter trait is referenced for routing",
                        "HTTP route handling is defined via filter trait",
                    ),
                ),
                (
                    r"warp::test::request\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP test request is created for route testing",
                        "HTTP endpoint is tested with mock request",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"warp::sse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SSE stream is exposed via Warp",
                        "Server-sent events are streamed for inbound clients",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"warp::ws\(\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket endpoint is exposed via Warp",
                        "WebSocket connections are handled for inbound clients",
                    ),
                ),
                (
                    r"warp::ws::Ws",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket upgrade is handled for connection",
                        "WebSocket connection is upgraded for inbound clients",
                    ),
                ),
            ],
        },
    },
)
