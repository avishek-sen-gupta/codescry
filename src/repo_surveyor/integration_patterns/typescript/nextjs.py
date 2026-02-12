"""Next.js framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Next.js"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"NextApiRequest", Confidence.HIGH),
            (r"NextApiResponse", Confidence.HIGH),
            (r"getServerSideProps", Confidence.HIGH),
            (r"getStaticProps", Confidence.HIGH),
            (r"from ['\"]next/server['\"]", Confidence.HIGH),
        ],
    },
}
