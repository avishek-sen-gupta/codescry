"""Common integration patterns that apply across all languages."""

from .types import Confidence, IntegrationType

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"(?i)\bhttp\b", Confidence.LOW),
            (r"(?i)\brest(?:ful)?\b", Confidence.LOW),
            (r"(?i)\bapi\b", Confidence.LOW),
            (r"(?i)\bendpoint\b", Confidence.LOW),
        ],
        "directory_patterns": [
            r"(?i)^controllers?$",
            r"(?i)^api$",
            r"(?i)^rest$",
            r"(?i)^endpoints?$",
            r"(?i)^resources?$",
            r"(?i)^handlers?$",
            r"(?i)^routes?$",
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            (r"(?i)\bsoap\b", Confidence.MEDIUM),
            (r"(?i)\bwsdl\b", Confidence.HIGH),
            (r"(?i)\benvelope\b", Confidence.LOW),
        ],
        "directory_patterns": [
            r"(?i)^soap$",
            r"(?i)^wsdl$",
            r"(?i)^webservices?$",
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"(?i)\bkafka\b", Confidence.HIGH),
            (r"(?i)\brabbit(?:mq)?\b", Confidence.HIGH),
            (r"(?i)\bamqp\b", Confidence.HIGH),
            (r"(?i)\bpulsar\b", Confidence.HIGH),
            (r"(?i)\bnats\b", Confidence.MEDIUM),
        ],
        "directory_patterns": [
            r"(?i)^messaging$",
            r"(?i)^kafka$",
            r"(?i)^rabbit(?:mq)?$",
            r"(?i)^queues?$",
            r"(?i)^events?$",
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"(?i)\bwebsocket\b", Confidence.HIGH),
            (r"(?i)\btcp\b", Confidence.MEDIUM),
            (r"(?i)\budp\b", Confidence.MEDIUM),
        ],
        "directory_patterns": [
            r"(?i)^sockets?$",
            r"(?i)^websockets?$",
        ],
    },
    IntegrationType.DATABASE: {
        "patterns": [
            (r"(?i)\bdatabase\b", Confidence.LOW),
            (r"(?i)\bdatasource\b", Confidence.MEDIUM),
        ],
        "directory_patterns": [
            r"(?i)^repositor(?:y|ies)$",
            r"(?i)^dao$",
            r"(?i)^database$",
            r"(?i)^db$",
            r"(?i)^persistence$",
            r"(?i)^models?$",
        ],
    },
}
