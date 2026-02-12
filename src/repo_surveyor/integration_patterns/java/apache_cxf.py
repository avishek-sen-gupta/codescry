"""Apache CXF framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Apache CXF"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"import org\.apache\.cxf", Confidence.HIGH),
            (r"JaxWsServerFactoryBean", Confidence.HIGH),
            (r"JaxRsServerFactoryBean", Confidence.HIGH),
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            (r"org\.apache\.cxf\.jaxws", Confidence.HIGH),
            (r"CxfEndpoint", Confidence.HIGH),
        ],
    },
}
