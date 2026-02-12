"""Next.js framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Next.js",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"NextApiRequest", Confidence.HIGH),
                (r"NextApiResponse", Confidence.HIGH),
                (r"getServerSideProps", Confidence.HIGH),
                (r"getStaticProps", Confidence.HIGH),
                (r"require\(['\"]next/server['\"]\)", Confidence.HIGH),
            ],
        },
    },
)
