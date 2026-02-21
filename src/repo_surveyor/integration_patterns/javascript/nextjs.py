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
                        "Next.js NextApiRequest type for API route request handling",
                        "This code uses Next.js to handle inbound API route requests",
                    ),
                ),
                (
                    r"NextApiResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Next.js NextApiResponse type for API route response handling",
                        "This code uses Next.js to handle inbound API route responses",
                    ),
                ),
                (
                    r"getServerSideProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Next.js getServerSideProps function for server-side data fetching",
                        "This code uses Next.js to handle inbound server-side page requests",
                    ),
                ),
                (
                    r"getStaticProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Next.js getStaticProps function for static page data fetching",
                        "This code uses Next.js to handle inbound static page generation requests",
                    ),
                ),
                (
                    r"require\(['\"]next/server['\"]\)",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Next.js next/server require for edge/middleware request handling",
                        "This code uses Next.js to handle inbound edge runtime requests",
                    ),
                ),
            ],
        },
    },
)
