"""Apache CXF framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Apache CXF",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import org\.apache\.cxf", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"JaxWsServerFactoryBean", Confidence.HIGH, SignalDirection.INWARD),
                (r"JaxRsServerFactoryBean", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"org\.apache\.cxf\.jaxws", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"CxfEndpoint", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
