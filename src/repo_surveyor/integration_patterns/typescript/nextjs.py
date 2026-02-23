"""Next.js framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Next.js",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"NextApiRequest",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "API requests are typed for handlers",
                        "HTTP API request is handled by route",
                    ),
                ),
                (
                    r"NextApiResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "API responses are typed for handlers",
                        "HTTP API request is handled by route",
                    ),
                ),
                (
                    r"getServerSideProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server rendering is performed via props",
                        "Server-side page is rendered for request",
                    ),
                ),
                (
                    r"getStaticProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Static generation is performed via props",
                        "Static page is served for request",
                    ),
                ),
                (
                    r"from ['\"]next/server['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Next.js server is imported for middleware routing",
                        "Server-side request is handled by framework",
                    ),
                ),
            ],
        },
    },
)
