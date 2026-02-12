"""Common integration patterns that apply across all languages."""

from .types import BasePatternSpec, Confidence, IntegrationType

COMMON = BasePatternSpec(
    patterns={
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
        IntegrationType.FILE_IO: {
            "patterns": [
                (r"(?i)\bfile\b", Confidence.LOW),
                (r"(?i)\bupload\b", Confidence.LOW),
                (r"(?i)\bdownload\b", Confidence.LOW),
            ],
            "directory_patterns": [
                r"(?i)^uploads?$",
                r"(?i)^files?$",
                r"(?i)^storage$",
                r"(?i)^assets$",
            ],
        },
        IntegrationType.GRPC: {
            "patterns": [
                (r"(?i)\bgrpc\b", Confidence.MEDIUM),
                (r"(?i)\bprotobuf\b", Confidence.MEDIUM),
            ],
            "directory_patterns": [
                r"(?i)^proto$",
                r"(?i)^grpc$",
                r"(?i)^protobuf$",
            ],
        },
    },
)
