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
                        "API request is handled by NextApiRequest",
                        "API route is handled for inbound requests",
                    ),
                ),
                (
                    r"NextApiResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "API response is typed for route handling",
                        "API route is handled for inbound responses",
                    ),
                ),
                (
                    r"getServerSideProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-side data is fetched for page rendering",
                        "Server-side page is handled for inbound requests",
                    ),
                ),
                (
                    r"getStaticProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Static page data is fetched at build time",
                        "Static page is generated for inbound requests",
                    ),
                ),
                (
                    r"require\(['\"]next/server['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Edge request is handled by middleware server",
                        "Edge runtime is handled for inbound requests",
                    ),
                ),
            ],
        },
    },
)
