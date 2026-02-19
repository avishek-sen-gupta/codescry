"""Next.js framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Next.js",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"NextApiRequest", Confidence.HIGH, SignalDirection.INWARD),
                (r"NextApiResponse", Confidence.HIGH, SignalDirection.INWARD),
                (r"getServerSideProps", Confidence.HIGH, SignalDirection.INWARD),
                (r"getStaticProps", Confidence.HIGH, SignalDirection.INWARD),
                (r"from ['\"]next/server['\"]", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
    },
)
