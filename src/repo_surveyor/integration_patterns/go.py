"""Go integration patterns."""

from .types import Confidence, IntegrationType

BASE_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
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
        "patterns": [
            (r'"github\.com/hooklift/gowsdl"', Confidence.HIGH),
            (r'"encoding/xml"', Confidence.MEDIUM),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r'"github\.com/Shopify/sarama"', Confidence.HIGH),
            (r'"github\.com/streadway/amqp"', Confidence.HIGH),
            (r'"github\.com/nats-io/nats\.go"', Confidence.HIGH),
            (r"sarama\.NewConsumer", Confidence.HIGH),
            (r"sarama\.NewProducer", Confidence.HIGH),
            (r"amqp\.Dial", Confidence.HIGH),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r'"github\.com/gorilla/websocket"', Confidence.HIGH),
            (r"websocket\.Upgrader", Confidence.HIGH),
            (r"net\.Listen\(", Confidence.MEDIUM),
            (r"net\.Dial\(", Confidence.MEDIUM),
        ],
    },
    IntegrationType.DATABASE: {
        "patterns": [
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
        "patterns": [
            (r"os\.Open\(", Confidence.HIGH),
            (r"os\.Create\(", Confidence.HIGH),
            (r"io\.Copy\(", Confidence.MEDIUM),
            (r"s3manager", Confidence.HIGH),
            (r'"github\.com/pkg/sftp"', Confidence.HIGH),
        ],
    },
    IntegrationType.GRPC: {
        "patterns": [
            (r'"google\.golang\.org/grpc"', Confidence.HIGH),
            (r"pb\.Register.*Server", Confidence.HIGH),
        ],
    },
}

FRAMEWORK_PATTERNS = {
    "Gin": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/gin-gonic/gin"', Confidence.HIGH),
                (r"gin\.Context", Confidence.HIGH),
                (r"gin\.Default\(", Confidence.HIGH),
                (r"gin\.New\(", Confidence.HIGH),
            ],
        },
    },
    "Echo": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/labstack/echo"', Confidence.HIGH),
                (r"echo\.Context", Confidence.HIGH),
                (r"echo\.New\(", Confidence.HIGH),
            ],
        },
    },
    "Fiber": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/gofiber/fiber"', Confidence.HIGH),
                (r"fiber\.Ctx", Confidence.HIGH),
                (r"fiber\.New\(", Confidence.HIGH),
            ],
        },
    },
    "Chi": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/go-chi/chi"', Confidence.HIGH),
                (r"chi\.NewRouter\(", Confidence.HIGH),
            ],
        },
    },
    "Gorilla": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r'"github\.com/gorilla/mux"', Confidence.HIGH),
                (r"mux\.NewRouter\(", Confidence.HIGH),
            ],
        },
    },
}
