"""Apache CXF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Apache CXF",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import org\.apache\.cxf", Confidence.HIGH),
                (r"JaxWsServerFactoryBean", Confidence.HIGH),
                (r"JaxRsServerFactoryBean", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"org\.apache\.cxf\.jaxws", Confidence.HIGH),
                (r"CxfEndpoint", Confidence.HIGH),
            ],
        },
    },
)
