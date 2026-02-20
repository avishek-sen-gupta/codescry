"""Go base integration patterns."""

from repo_surveyor.integration_patterns.types import (
    BasePatternSpec,
    Confidence,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'"net/http"', Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"http\.HandleFunc", Confidence.HIGH, SignalDirection.INWARD),
                (r"http\.Handle\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\.GET\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\.POST\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\.PUT\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\.DELETE\(", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/hooklift/gowsdl"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r'"encoding/xml"', Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/Shopify/sarama"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r'"github\.com/streadway/amqp"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r'"github\.com/nats-io/nats\.go"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"sarama\.NewConsumer", Confidence.HIGH, SignalDirection.INWARD),
                (r"sarama\.NewProducer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"amqp\.Dial", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r'"cloud\.google\.com/go/pubsub"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/gorilla/websocket"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"websocket\.Upgrader", Confidence.HIGH, SignalDirection.INWARD),
                (r"net\.Listen\(", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"net\.Dial\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r'"gorm\.io/gorm"', Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'"github\.com/jmoiron/sqlx"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r'"github\.com/jackc/pgx"', Confidence.HIGH, SignalDirection.OUTWARD),
                (r'"database/sql"', Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"gorm\.Model", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.Create\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.Find\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"db\.Where\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"sql\.Open\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'"github\.com/gocql/gocql"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r'"cloud\.google\.com/go/firestore"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r'"github\.com/neo4j/neo4j-go-driver"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"neo4j\.NewDriverWithContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"os\.Open\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"os\.Create\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"io\.Copy\(", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"s3manager", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'"cloud\.google\.com/go/storage"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r'"google\.golang\.org/grpc"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"pb\.Register.*Server", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/99designs/gqlgen"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r'"github\.com/graphql-go/graphql"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r'"net/smtp"', Confidence.HIGH, SignalDirection.OUTWARD),
                (r'"gopkg\.in/gomail"', Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/go-redis/redis"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r'"github\.com/bradfitz/gomemcache"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"text/event-stream", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"http\.Flusher", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r'"github\.com/pkg/sftp"', Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'"github\.com/jlaffaye/ftp"',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r'"github\.com/robfig/cron"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"time\.NewTicker", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
