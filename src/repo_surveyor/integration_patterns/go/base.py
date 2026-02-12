"""Go base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'"net/http"', Confidence.MEDIUM),
                (r"http\.HandleFunc", Confidence.HIGH),
                (r"http\.Handle\(", Confidence.HIGH),
                (r"\.GET\(", Confidence.HIGH),
                (r"\.POST\(", Confidence.HIGH),
                (r"\.PUT\(", Confidence.HIGH),
                (r"\.DELETE\(", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r'"github\.com/hooklift/gowsdl"', Confidence.HIGH),
                (r'"encoding/xml"', Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r'"github\.com/Shopify/sarama"', Confidence.HIGH),
                (r'"github\.com/streadway/amqp"', Confidence.HIGH),
                (r'"github\.com/nats-io/nats\.go"', Confidence.HIGH),
                (r"sarama\.NewConsumer", Confidence.HIGH),
                (r"sarama\.NewProducer", Confidence.HIGH),
                (r"amqp\.Dial", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r'"github\.com/gorilla/websocket"', Confidence.HIGH),
                (r"websocket\.Upgrader", Confidence.HIGH),
                (r"net\.Listen\(", Confidence.MEDIUM),
                (r"net\.Dial\(", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r'"gorm\.io/gorm"', Confidence.HIGH),
                (r'"github\.com/jmoiron/sqlx"', Confidence.HIGH),
                (r'"github\.com/jackc/pgx"', Confidence.HIGH),
                (r'"database/sql"', Confidence.MEDIUM),
                (r"gorm\.Model", Confidence.HIGH),
                (r"db\.Create\(", Confidence.HIGH),
                (r"db\.Find\(", Confidence.HIGH),
                (r"db\.Where\(", Confidence.HIGH),
                (r"sql\.Open\(", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"os\.Open\(", Confidence.HIGH),
                (r"os\.Create\(", Confidence.HIGH),
                (r"io\.Copy\(", Confidence.MEDIUM),
                (r"s3manager", Confidence.HIGH),
                (r'"github\.com/pkg/sftp"', Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r'"google\.golang\.org/grpc"', Confidence.HIGH),
                (r"pb\.Register.*Server", Confidence.HIGH),
            ],
        },
    },
)
