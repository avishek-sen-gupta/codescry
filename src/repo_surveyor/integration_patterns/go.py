"""Go integration patterns."""

from .types import Confidence, IntegrationType

PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r'"github\.com/gin-gonic/gin"', Confidence.HIGH),
            (r'"github\.com/labstack/echo"', Confidence.HIGH),
            (r'"github\.com/gofiber/fiber"', Confidence.HIGH),
            (r'"github\.com/go-chi/chi"', Confidence.HIGH),
            (r'"github\.com/gorilla/mux"', Confidence.HIGH),
            (r'"net/http"', Confidence.MEDIUM),
            (r"http\.HandleFunc", Confidence.HIGH),
            (r"http\.Handle\(", Confidence.HIGH),
            (r"gin\.Context", Confidence.HIGH),
            (r"echo\.Context", Confidence.HIGH),
            (r"fiber\.Ctx", Confidence.HIGH),
            (r"\.GET\(", Confidence.HIGH),
            (r"\.POST\(", Confidence.HIGH),
            (r"\.PUT\(", Confidence.HIGH),
            (r"\.DELETE\(", Confidence.HIGH),
            (r"r\.HandleFunc\(", Confidence.HIGH),
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [],
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
}
