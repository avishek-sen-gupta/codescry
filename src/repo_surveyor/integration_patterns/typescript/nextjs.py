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
                        "TypeScript Next.js NextApiRequest type for inbound API route handler",
                        "This code uses TypeScript Next.js to handle inbound HTTP API requests",
                    ),
                ),
                (
                    r"NextApiResponse",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Next.js NextApiResponse type for API route response",
                        "This code uses TypeScript Next.js to handle inbound HTTP API requests",
                    ),
                ),
                (
                    r"getServerSideProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Next.js getServerSideProps function for server-side rendering",
                        "This code uses TypeScript Next.js to handle inbound server-side page requests",
                    ),
                ),
                (
                    r"getStaticProps",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Next.js getStaticProps function for static site generation",
                        "This code uses TypeScript Next.js to handle inbound static page requests",
                    ),
                ),
                (
                    r"from ['\"]next/server['\"]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "TypeScript Next.js server import for edge middleware and API routes",
                        "This code uses TypeScript Next.js to handle inbound server-side requests",
                    ),
                ),
            ],
        },
    },
)
