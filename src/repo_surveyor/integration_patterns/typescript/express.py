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
                        "TypeScript Express import for HTTP server framework",
                        "This code uses TypeScript Express to expose an inbound HTTP server",
                    ),
                ),
                (
                    r"\w*\.get\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Express .get() route handler accepting inbound GET requests",
                        "This code uses TypeScript Express to handle inbound GET HTTP requests",
                    ),
                ),
                (
                    r"\w*\.post\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Express .post() route handler accepting inbound POST requests",
                        "This code uses TypeScript Express to handle inbound POST HTTP requests",
                    ),
                ),
                (
                    r"\w*\.put\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Express .put() route handler accepting inbound PUT requests",
                        "This code uses TypeScript Express to handle inbound PUT HTTP requests",
                    ),
                ),
                (
                    r"\w*\.delete\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Express .delete() route handler accepting inbound DELETE requests",
                        "This code uses TypeScript Express to handle inbound DELETE HTTP requests",
                    ),
                ),
            ],
        },
    },
)
