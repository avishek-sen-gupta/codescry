"""Express framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Express",
    import_patterns=(r"from ['\"]express['\"]",),
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"from ['\"]express['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via Express",
                        "HTTP server is exposed for inbound requests",
                    ),
                ),
                (
                    r"\w*\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GET requests are handled by route",
                        "GET request is handled by HTTP route",
                    ),
                ),
                (
                    r"\w*\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "POST requests are handled by route",
                        "POST request is handled by HTTP route",
                    ),
                ),
                (
                    r"\w*\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "PUT requests are handled by route",
                        "PUT request is handled by HTTP route",
                    ),
                ),
                (
                    r"\w*\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "DELETE requests are handled by route",
                        "DELETE request is handled by HTTP route",
                    ),
                ),
            ],
        },
    },
)
