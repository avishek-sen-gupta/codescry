"""Angular framework integration patterns."""

from ..types import Confidence, IntegrationType

NAME = "Angular"

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"from ['\"]@angular/common/http['\"]", Confidence.HIGH),
            (r"HttpClient", Confidence.HIGH),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"from ['\"]@angular/.*websocket", Confidence.MEDIUM),
        ],
    },
}
