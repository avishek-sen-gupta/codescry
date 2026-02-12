"""Next.js framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Next.js",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"NextApiRequest", Confidence.HIGH),
                (r"NextApiResponse", Confidence.HIGH),
                (r"getServerSideProps", Confidence.HIGH),
                (r"getStaticProps", Confidence.HIGH),
                (r"from ['\"]next/server['\"]", Confidence.HIGH),
            ],
        },
    },
)
