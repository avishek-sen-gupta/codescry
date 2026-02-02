"""Integration point detection from CTags output.

Detects system integration points (HTTP, SOAP, messaging, sockets, database)
from code symbols using text search and regex heuristics.

Patterns are organized by language to support language-specific conventions.
"""

import re
from dataclasses import dataclass
from enum import Enum
from itertools import chain
from pathlib import Path
from typing import Iterator


from .ctags import CTagsEntry, CTagsResult


class IntegrationType(Enum):
    """Types of system integrations that can be detected."""

    HTTP_REST = "http_rest"
    SOAP = "soap"
    MESSAGING = "messaging"
    SOCKET = "socket"
    DATABASE = "database"


class Confidence(Enum):
    """Confidence levels for integration point detection."""

    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


class EntityType(Enum):
    """Type of entity where integration was detected."""

    CODE_SYMBOL = "code_symbol"
    DIRECTORY = "directory"


@dataclass(frozen=True)
class IntegrationPoint:
    """A detected integration point."""

    entry: CTagsEntry
    integration_type: IntegrationType
    confidence: Confidence
    matched_pattern: str
    entity_type: EntityType


@dataclass(frozen=True)
class IntegrationDetectorResult:
    """Result of integration detection."""

    integration_points: list[IntegrationPoint]
    entries_scanned: int


# Common patterns that apply across all languages
COMMON_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "name_patterns": [
            r"(?i)http",
            r"(?i)rest(?:ful)?",
            r"(?i)api(?:client|handler)?",
            r"(?i)endpoint",
            r"(?i)web(?:client|service)",
        ],
        "signature_patterns": [],
        "scope_patterns": [
            r"(?i)controller",
            r"(?i)resource",
        ],
        "directory_patterns": [
            r"(?i)^controllers?$",
            r"(?i)^api$",
            r"(?i)^rest$",
            r"(?i)^endpoints?$",
            r"(?i)^resources?$",
            r"(?i)^web$",
            r"(?i)^http$",
            r"(?i)^handlers?$",
            r"(?i)^routes?$",
        ],
    },
    IntegrationType.SOAP: {
        "name_patterns": [
            r"(?i)soap",
            r"(?i)wsdl",
            r"(?i)xml(?:service|client|handler)",
            r"(?i)webservice",
            r"(?i)envelope",
        ],
        "signature_patterns": [],
        "scope_patterns": [
            r"(?i)soapservice",
        ],
        "directory_patterns": [
            r"(?i)^soap$",
            r"(?i)^wsdl$",
            r"(?i)^webservices?$",
            r"(?i)^ws$",
        ],
    },
    IntegrationType.MESSAGING: {
        "name_patterns": [
            r"(?i)kafka",
            r"(?i)rabbit(?:mq)?",
            r"(?i)queue",
            r"(?i)topic",
            r"(?i)publisher",
            r"(?i)subscriber",
            r"(?i)event(?:handler|listener|publisher|bus)?",
            r"(?i)amqp",
            r"(?i)pulsar",
            r"(?i)nats",
        ],
        "signature_patterns": [],
        "scope_patterns": [
            r"(?i)eventhandler",
            r"(?i)subscriber",
        ],
        "directory_patterns": [
            r"(?i)^messaging$",
            r"(?i)^kafka$",
            r"(?i)^rabbit(?:mq)?$",
            r"(?i)^queues?$",
            r"(?i)^events?$",
            r"(?i)^publishers?$",
            r"(?i)^subscribers?$",
            r"(?i)^listeners?$",
        ],
    },
    IntegrationType.SOCKET: {
        "name_patterns": [
            r"(?i)socket",
            r"(?i)websocket",
            r"(?i)tcp",
            r"(?i)udp",
        ],
        "signature_patterns": [],
        "scope_patterns": [
            r"(?i)websockethandler",
            r"(?i)socketserver",
            r"(?i)socketclient",
        ],
        "directory_patterns": [
            r"(?i)^sockets?$",
            r"(?i)^websockets?$",
            r"(?i)^tcp$",
            r"(?i)^udp$",
        ],
    },
    IntegrationType.DATABASE: {
        "name_patterns": [
            r"(?i)repository",
            r"(?i)dao",
            r"(?i)database",
            r"(?i)datasource",
            r"(?i)query",
            r"(?i)crud",
            r"(?i)persist",
        ],
        "signature_patterns": [],
        "scope_patterns": [
            r"(?i)repository",
            r"(?i)dao",
        ],
        "directory_patterns": [
            r"(?i)^repositor(?:y|ies)$",
            r"(?i)^dao$",
            r"(?i)^database$",
            r"(?i)^db$",
            r"(?i)^persistence$",
            r"(?i)^entities$",
            r"(?i)^models?$",
            r"(?i)^mappers?$",
        ],
    },
}

# Language-specific patterns
LANGUAGE_PATTERNS = {
    "Java": {
        IntegrationType.HTTP_REST: {
            "name_patterns": [
                r"(?i)request(?:handler|mapping)?",
                r"(?i)response",
                r"(?i)get(?:mapping|request)",
                r"(?i)post(?:mapping|request)",
                r"(?i)put(?:mapping|request)",
                r"(?i)delete(?:mapping|request)",
                r"(?i)patch(?:mapping|request)",
                r"(?i)servlet",
            ],
            "signature_patterns": [
                r"@RequestMapping",
                r"@GetMapping",
                r"@PostMapping",
                r"@PutMapping",
                r"@DeleteMapping",
                r"@PatchMapping",
                r"@RestController",
                r"@Controller",
                r"@RequestBody",
                r"@ResponseBody",
                r"@PathVariable",
                r"@RequestParam",
                r"HttpServletRequest",
                r"HttpServletResponse",
                r"@GET",
                r"@POST",
                r"@PUT",
                r"@DELETE",
                r"@Path\(",
                r"@Produces",
                r"@Consumes",
            ],
            "scope_patterns": [
                r"(?i)restcontroller",
                r"(?i)apicontroller",
                r"(?i)webcontroller",
            ],
        },
        IntegrationType.SOAP: {
            "name_patterns": [
                r"(?i)porttype",
                r"(?i)binding",
                r"(?i)soapaction",
            ],
            "signature_patterns": [
                r"@WebService",
                r"@WebMethod",
                r"@WebParam",
                r"@SOAPBinding",
                r"@WebResult",
                r"SOAPMessage",
                r"SOAPEnvelope",
                r"SOAPBody",
                r"SOAPHeader",
                r"JAXBContext",
                r"Marshaller",
                r"Unmarshaller",
            ],
            "scope_patterns": [
                r"(?i)webserviceimpl",
                r"(?i)portimpl",
            ],
        },
        IntegrationType.MESSAGING: {
            "name_patterns": [
                r"(?i)jms",
                r"(?i)message(?:handler|listener|producer|consumer|sender|receiver)?",
                r"(?i)activemq",
            ],
            "signature_patterns": [
                r"@KafkaListener",
                r"@JmsListener",
                r"@RabbitListener",
                r"@StreamListener",
                r"@SendTo",
                r"@EnableKafka",
                r"@EnableJms",
                r"@EnableRabbit",
                r"KafkaTemplate",
                r"JmsTemplate",
                r"RabbitTemplate",
                r"MessageChannel",
                r"MessageListener",
                r"MessageProducer",
                r"MessageConsumer",
                r"ConnectionFactory",
            ],
            "scope_patterns": [
                r"(?i)kafkaconsumer",
                r"(?i)kafkaproducer",
                r"(?i)messagelistener",
            ],
        },
        IntegrationType.SOCKET: {
            "name_patterns": [
                r"(?i)netty",
                r"(?i)nio",
                r"(?i)channel",
                r"(?i)serverchannel",
                r"(?i)socketchannel",
            ],
            "signature_patterns": [
                r"@ServerEndpoint",
                r"@OnOpen",
                r"@OnClose",
                r"@OnMessage",
                r"@OnError",
                r"@EnableWebSocket",
                r"WebSocketHandler",
                r"WebSocketSession",
                r"ServerSocket",
                r"DatagramSocket",
                r"SocketChannel",
                r"ServerSocketChannel",
                r"Selector",
                r"SelectionKey",
            ],
            "scope_patterns": [
                r"(?i)channelhandler",
            ],
        },
        IntegrationType.DATABASE: {
            "name_patterns": [
                r"(?i)jdbc",
                r"(?i)entitymanager",
                r"(?i)session(?:factory)?",
                r"(?i)transaction",
                r"(?i)hibernate",
                r"(?i)mybatis",
                r"(?i)jpa",
            ],
            "signature_patterns": [
                r"@Repository",
                r"@Entity",
                r"@Table",
                r"@Query",
                r"@Transactional",
                r"@PersistenceContext",
                r"@Id",
                r"@Column",
                r"@JoinColumn",
                r"@OneToMany",
                r"@ManyToOne",
                r"@ManyToMany",
                r"@OneToOne",
                r"Connection",
                r"PreparedStatement",
                r"ResultSet",
                r"Statement",
                r"DataSource",
                r"EntityManager",
                r"SessionFactory",
                r"JdbcTemplate",
                r"NamedParameterJdbcTemplate",
            ],
            "scope_patterns": [
                r"(?i)mapper",
                r"(?i)entitymanager",
            ],
        },
    },
    "Rust": {
        IntegrationType.HTTP_REST: {
            "name_patterns": [
                r"(?i)actix",
                r"(?i)axum",
                r"(?i)rocket",
                r"(?i)warp",
                r"(?i)hyper",
                r"(?i)reqwest",
                r"(?i)handler",
                r"(?i)router",
                r"(?i)route",
            ],
            "signature_patterns": [
                r"#\[get\(",
                r"#\[post\(",
                r"#\[put\(",
                r"#\[delete\(",
                r"#\[patch\(",
                r"#\[route\(",
                r"#\[api\(",
                r"HttpRequest",
                r"HttpResponse",
                r"web::Json",
                r"web::Path",
                r"web::Query",
                r"web::Data",
                r"web::Form",
                r"Json<",
                r"Path<",
                r"Query<",
                r"State<",
                r"Extension<",
                r"Router::",
                r"\.route\(",
            ],
            "scope_patterns": [
                r"(?i)handler",
                r"(?i)controller",
            ],
        },
        IntegrationType.SOAP: {
            "name_patterns": [],
            "signature_patterns": [],
            "scope_patterns": [],
        },
        IntegrationType.MESSAGING: {
            "name_patterns": [
                r"(?i)rdkafka",
                r"(?i)lapin",
                r"(?i)tokio_amqp",
            ],
            "signature_patterns": [
                r"StreamConsumer",
                r"FutureProducer",
                r"BaseConsumer",
                r"BaseProducer",
            ],
            "scope_patterns": [],
        },
        IntegrationType.SOCKET: {
            "name_patterns": [
                r"(?i)tokio",
                r"(?i)tungstenite",
                r"(?i)async_std",
            ],
            "signature_patterns": [
                r"TcpListener",
                r"TcpStream",
                r"UdpSocket",
                r"WebSocketStream",
                r"tokio::net::",
                r"async_std::net::",
            ],
            "scope_patterns": [],
        },
        IntegrationType.DATABASE: {
            "name_patterns": [
                r"(?i)diesel",
                r"(?i)sqlx",
                r"(?i)sea_orm",
                r"(?i)tokio_postgres",
                r"(?i)rusqlite",
            ],
            "signature_patterns": [
                r"#\[derive\(.*Queryable",
                r"#\[derive\(.*Insertable",
                r"#\[derive\(.*Selectable",
                r"#\[table_name",
                r"#\[diesel\(",
                r"PgConnection",
                r"SqliteConnection",
                r"MysqlConnection",
                r"Pool<",
                r"query_as!",
                r"query!",
                r"sqlx::query",
            ],
            "scope_patterns": [],
        },
    },
    "Python": {
        IntegrationType.HTTP_REST: {
            "name_patterns": [
                r"(?i)flask",
                r"(?i)django",
                r"(?i)fastapi",
                r"(?i)starlette",
                r"(?i)aiohttp",
                r"(?i)requests",
                r"(?i)httpx",
                r"(?i)view",
            ],
            "signature_patterns": [
                r"@app\.route",
                r"@app\.get",
                r"@app\.post",
                r"@app\.put",
                r"@app\.delete",
                r"@router\.get",
                r"@router\.post",
                r"@api_view",
                r"@action",
                r"APIView",
                r"ViewSet",
                r"GenericAPIView",
                r"Request",
                r"Response",
            ],
            "scope_patterns": [
                r"(?i)view",
                r"(?i)viewset",
            ],
        },
        IntegrationType.SOAP: {
            "name_patterns": [
                r"(?i)zeep",
                r"(?i)suds",
            ],
            "signature_patterns": [
                r"Client\(",
                r"zeep\.Client",
            ],
            "scope_patterns": [],
        },
        IntegrationType.MESSAGING: {
            "name_patterns": [
                r"(?i)celery",
                r"(?i)kombu",
                r"(?i)pika",
                r"(?i)aiokafka",
                r"(?i)kafka_python",
            ],
            "signature_patterns": [
                r"@app\.task",
                r"@celery\.task",
                r"@shared_task",
                r"KafkaConsumer",
                r"KafkaProducer",
            ],
            "scope_patterns": [],
        },
        IntegrationType.SOCKET: {
            "name_patterns": [
                r"(?i)websockets?",
                r"(?i)socketio",
                r"(?i)asyncio",
            ],
            "signature_patterns": [
                r"@socketio\.on",
                r"@sio\.on",
                r"websocket_connect",
                r"WebSocket",
            ],
            "scope_patterns": [],
        },
        IntegrationType.DATABASE: {
            "name_patterns": [
                r"(?i)sqlalchemy",
                r"(?i)peewee",
                r"(?i)tortoise",
                r"(?i)django\.db",
                r"(?i)psycopg",
                r"(?i)pymysql",
                r"(?i)asyncpg",
            ],
            "signature_patterns": [
                r"@declarative",
                r"Column\(",
                r"relationship\(",
                r"ForeignKey\(",
                r"Base\.metadata",
                r"Session\(",
                r"models\.Model",
                r"CharField",
                r"IntegerField",
                r"TextField",
            ],
            "scope_patterns": [
                r"(?i)model",
            ],
        },
    },
    "TypeScript": {
        IntegrationType.HTTP_REST: {
            "name_patterns": [
                r"(?i)express",
                r"(?i)nestjs",
                r"(?i)fastify",
                r"(?i)koa",
                r"(?i)hapi",
                r"(?i)axios",
                r"(?i)fetch",
            ],
            "signature_patterns": [
                r"@Controller\(",
                r"@Get\(",
                r"@Post\(",
                r"@Put\(",
                r"@Delete\(",
                r"@Patch\(",
                r"@Body\(",
                r"@Param\(",
                r"@Query\(",
                r"app\.get\(",
                r"app\.post\(",
                r"router\.get\(",
                r"router\.post\(",
                r"Request",
                r"Response",
            ],
            "scope_patterns": [
                r"(?i)controller",
            ],
        },
        IntegrationType.SOAP: {
            "name_patterns": [
                r"(?i)soap",
            ],
            "signature_patterns": [],
            "scope_patterns": [],
        },
        IntegrationType.MESSAGING: {
            "name_patterns": [
                r"(?i)kafkajs",
                r"(?i)amqplib",
                r"(?i)bull",
            ],
            "signature_patterns": [
                r"@MessagePattern\(",
                r"@EventPattern\(",
                r"Kafka",
                r"Consumer",
                r"Producer",
            ],
            "scope_patterns": [],
        },
        IntegrationType.SOCKET: {
            "name_patterns": [
                r"(?i)socket\.io",
                r"(?i)ws",
            ],
            "signature_patterns": [
                r"@WebSocketGateway",
                r"@SubscribeMessage",
                r"io\.on\(",
                r"socket\.on\(",
                r"WebSocket",
            ],
            "scope_patterns": [],
        },
        IntegrationType.DATABASE: {
            "name_patterns": [
                r"(?i)typeorm",
                r"(?i)prisma",
                r"(?i)sequelize",
                r"(?i)mongoose",
                r"(?i)knex",
            ],
            "signature_patterns": [
                r"@Entity\(",
                r"@Column\(",
                r"@PrimaryGeneratedColumn",
                r"@ManyToOne",
                r"@OneToMany",
                r"@Repository\(",
                r"prisma\.",
                r"Schema\(",
            ],
            "scope_patterns": [
                r"(?i)repository",
                r"(?i)entity",
            ],
        },
    },
    "JavaScript": {
        IntegrationType.HTTP_REST: {
            "name_patterns": [
                r"(?i)express",
                r"(?i)fastify",
                r"(?i)koa",
                r"(?i)hapi",
                r"(?i)axios",
                r"(?i)fetch",
            ],
            "signature_patterns": [
                r"app\.get\(",
                r"app\.post\(",
                r"app\.put\(",
                r"app\.delete\(",
                r"router\.get\(",
                r"router\.post\(",
                r"req,\s*res",
            ],
            "scope_patterns": [],
        },
        IntegrationType.SOAP: {
            "name_patterns": [
                r"(?i)soap",
            ],
            "signature_patterns": [],
            "scope_patterns": [],
        },
        IntegrationType.MESSAGING: {
            "name_patterns": [
                r"(?i)kafkajs",
                r"(?i)amqplib",
                r"(?i)bull",
            ],
            "signature_patterns": [
                r"consumer\.run\(",
                r"producer\.send\(",
            ],
            "scope_patterns": [],
        },
        IntegrationType.SOCKET: {
            "name_patterns": [
                r"(?i)socket\.io",
                r"(?i)ws",
            ],
            "signature_patterns": [
                r"io\.on\(",
                r"socket\.on\(",
                r"WebSocket",
            ],
            "scope_patterns": [],
        },
        IntegrationType.DATABASE: {
            "name_patterns": [
                r"(?i)sequelize",
                r"(?i)mongoose",
                r"(?i)knex",
                r"(?i)mongodb",
            ],
            "signature_patterns": [
                r"Schema\(",
                r"model\(",
                r"define\(",
            ],
            "scope_patterns": [
                r"(?i)model",
            ],
        },
    },
    "Go": {
        IntegrationType.HTTP_REST: {
            "name_patterns": [
                r"(?i)gin",
                r"(?i)echo",
                r"(?i)fiber",
                r"(?i)chi",
                r"(?i)mux",
                r"(?i)handler",
            ],
            "signature_patterns": [
                r"http\.Handler",
                r"http\.HandleFunc",
                r"gin\.Context",
                r"echo\.Context",
                r"fiber\.Ctx",
                r"\.GET\(",
                r"\.POST\(",
                r"\.PUT\(",
                r"\.DELETE\(",
                r"r\.HandleFunc\(",
            ],
            "scope_patterns": [
                r"(?i)handler",
            ],
        },
        IntegrationType.SOAP: {
            "name_patterns": [],
            "signature_patterns": [],
            "scope_patterns": [],
        },
        IntegrationType.MESSAGING: {
            "name_patterns": [
                r"(?i)sarama",
                r"(?i)amqp",
                r"(?i)nats",
            ],
            "signature_patterns": [
                r"sarama\.Consumer",
                r"sarama\.Producer",
                r"amqp\.Channel",
            ],
            "scope_patterns": [],
        },
        IntegrationType.SOCKET: {
            "name_patterns": [
                r"(?i)gorilla",
                r"(?i)websocket",
            ],
            "signature_patterns": [
                r"websocket\.Upgrader",
                r"websocket\.Conn",
                r"net\.Listen",
                r"net\.Dial",
            ],
            "scope_patterns": [],
        },
        IntegrationType.DATABASE: {
            "name_patterns": [
                r"(?i)gorm",
                r"(?i)sqlx",
                r"(?i)pgx",
            ],
            "signature_patterns": [
                r"gorm\.Model",
                r"db\.Create\(",
                r"db\.Find\(",
                r"db\.Where\(",
                r"sql\.DB",
                r"sql\.Open\(",
            ],
            "scope_patterns": [
                r"(?i)repository",
            ],
        },
    },
    "C#": {
        IntegrationType.HTTP_REST: {
            "name_patterns": [
                r"(?i)controller",
                r"(?i)apicontroller",
            ],
            "signature_patterns": [
                r"\[ApiController\]",
                r"\[HttpGet\]",
                r"\[HttpPost\]",
                r"\[HttpPut\]",
                r"\[HttpDelete\]",
                r"\[Route\(",
                r"\[FromBody\]",
                r"\[FromQuery\]",
                r"IActionResult",
                r"ActionResult",
                r"ControllerBase",
            ],
            "scope_patterns": [
                r"(?i)controller",
            ],
        },
        IntegrationType.SOAP: {
            "name_patterns": [
                r"(?i)wcf",
                r"(?i)servicecontract",
            ],
            "signature_patterns": [
                r"\[ServiceContract\]",
                r"\[OperationContract\]",
                r"\[DataContract\]",
                r"\[DataMember\]",
            ],
            "scope_patterns": [],
        },
        IntegrationType.MESSAGING: {
            "name_patterns": [
                r"(?i)masstransit",
                r"(?i)nservicebus",
                r"(?i)rebus",
            ],
            "signature_patterns": [
                r"IConsumer<",
                r"IMessageHandler",
                r"IBus",
            ],
            "scope_patterns": [],
        },
        IntegrationType.SOCKET: {
            "name_patterns": [
                r"(?i)signalr",
            ],
            "signature_patterns": [
                r"Hub<",
                r"HubConnection",
                r"TcpClient",
                r"TcpListener",
                r"WebSocket",
            ],
            "scope_patterns": [],
        },
        IntegrationType.DATABASE: {
            "name_patterns": [
                r"(?i)entityframework",
                r"(?i)dbcontext",
                r"(?i)dapper",
            ],
            "signature_patterns": [
                r"DbContext",
                r"DbSet<",
                r"\[Table\(",
                r"\[Key\]",
                r"\[Column\(",
                r"IRepository",
                r"SqlConnection",
            ],
            "scope_patterns": [
                r"(?i)repository",
                r"(?i)context",
            ],
        },
    },
}

# High confidence patterns by language
HIGH_CONFIDENCE_PATTERNS = {
    "common": {
        IntegrationType.HTTP_REST: [],
        IntegrationType.SOAP: [],
        IntegrationType.MESSAGING: [],
        IntegrationType.SOCKET: [],
        IntegrationType.DATABASE: [],
    },
    "Java": {
        IntegrationType.HTTP_REST: [
            r"@RequestMapping",
            r"@GetMapping",
            r"@PostMapping",
            r"@RestController",
            r"@Controller",
            r"@GET",
            r"@POST",
            r"@Path\(",
        ],
        IntegrationType.SOAP: [
            r"@WebService",
            r"@WebMethod",
            r"SOAPMessage",
            r"SOAPEnvelope",
        ],
        IntegrationType.MESSAGING: [
            r"@KafkaListener",
            r"@JmsListener",
            r"@RabbitListener",
            r"KafkaTemplate",
            r"JmsTemplate",
            r"RabbitTemplate",
        ],
        IntegrationType.SOCKET: [
            r"@ServerEndpoint",
            r"@OnOpen",
            r"@OnMessage",
            r"WebSocketHandler",
            r"ServerSocket",
        ],
        IntegrationType.DATABASE: [
            r"@Repository",
            r"@Entity",
            r"@Query",
            r"@Transactional",
            r"EntityManager",
            r"JdbcTemplate",
            r"PreparedStatement",
        ],
    },
    "Rust": {
        IntegrationType.HTTP_REST: [
            r"#\[get\(",
            r"#\[post\(",
            r"#\[put\(",
            r"#\[delete\(",
            r"HttpResponse",
            r"web::Json",
        ],
        IntegrationType.SOAP: [],
        IntegrationType.MESSAGING: [
            r"StreamConsumer",
            r"FutureProducer",
        ],
        IntegrationType.SOCKET: [
            r"TcpListener",
            r"WebSocketStream",
        ],
        IntegrationType.DATABASE: [
            r"#\[derive\(.*Queryable",
            r"#\[diesel\(",
            r"sqlx::query",
            r"PgConnection",
        ],
    },
    "Python": {
        IntegrationType.HTTP_REST: [
            r"@app\.route",
            r"@app\.get",
            r"@app\.post",
            r"@router\.get",
            r"@api_view",
            r"APIView",
        ],
        IntegrationType.SOAP: [],
        IntegrationType.MESSAGING: [
            r"@app\.task",
            r"@celery\.task",
            r"KafkaConsumer",
            r"KafkaProducer",
        ],
        IntegrationType.SOCKET: [
            r"@socketio\.on",
            r"WebSocket",
        ],
        IntegrationType.DATABASE: [
            r"@declarative",
            r"models\.Model",
            r"Session\(",
        ],
    },
    "TypeScript": {
        IntegrationType.HTTP_REST: [
            r"@Controller\(",
            r"@Get\(",
            r"@Post\(",
        ],
        IntegrationType.SOAP: [],
        IntegrationType.MESSAGING: [
            r"@MessagePattern\(",
            r"@EventPattern\(",
        ],
        IntegrationType.SOCKET: [
            r"@WebSocketGateway",
            r"@SubscribeMessage",
        ],
        IntegrationType.DATABASE: [
            r"@Entity\(",
            r"@Repository\(",
            r"prisma\.",
        ],
    },
    "JavaScript": {
        IntegrationType.HTTP_REST: [
            r"app\.get\(",
            r"app\.post\(",
            r"router\.get\(",
        ],
        IntegrationType.SOAP: [],
        IntegrationType.MESSAGING: [],
        IntegrationType.SOCKET: [
            r"io\.on\(",
            r"socket\.on\(",
        ],
        IntegrationType.DATABASE: [
            r"Schema\(",
            r"model\(",
        ],
    },
    "Go": {
        IntegrationType.HTTP_REST: [
            r"http\.HandleFunc",
            r"gin\.Context",
            r"\.GET\(",
            r"\.POST\(",
        ],
        IntegrationType.SOAP: [],
        IntegrationType.MESSAGING: [
            r"sarama\.Consumer",
            r"sarama\.Producer",
        ],
        IntegrationType.SOCKET: [
            r"websocket\.Upgrader",
            r"net\.Listen",
        ],
        IntegrationType.DATABASE: [
            r"gorm\.Model",
            r"db\.Create\(",
            r"sql\.Open\(",
        ],
    },
    "C#": {
        IntegrationType.HTTP_REST: [
            r"\[ApiController\]",
            r"\[HttpGet\]",
            r"\[HttpPost\]",
        ],
        IntegrationType.SOAP: [
            r"\[ServiceContract\]",
            r"\[OperationContract\]",
        ],
        IntegrationType.MESSAGING: [
            r"IConsumer<",
        ],
        IntegrationType.SOCKET: [
            r"Hub<",
            r"HubConnection",
        ],
        IntegrationType.DATABASE: [
            r"DbContext",
            r"DbSet<",
        ],
    },
}

# Strong keywords that indicate medium confidence for name matches
STRONG_KEYWORDS = [
    "controller",
    "repository",
    "listener",
    "handler",
    "template",
]


def _merge_patterns(
    common: dict[str, list[str]], language_specific: dict[str, list[str]]
) -> dict[str, list[str]]:
    """Merge common patterns with language-specific patterns.

    Args:
        common: Common patterns dict.
        language_specific: Language-specific patterns dict.

    Returns:
        Merged patterns dict with combined lists.
    """
    return {
        key: common.get(key, []) + language_specific.get(key, [])
        for key in set(common.keys()) | set(language_specific.keys())
    }


def get_patterns_for_language(
    language: str,
) -> dict[IntegrationType, dict[str, list[str]]]:
    """Get integration patterns for a specific language.

    Args:
        language: The programming language (e.g., "Java", "Rust", "Python").

    Returns:
        Dict mapping IntegrationType to pattern dicts.
    """
    language_patterns = LANGUAGE_PATTERNS.get(language, {})

    return {
        integration_type: _merge_patterns(
            common_patterns, language_patterns.get(integration_type, {})
        )
        for integration_type, common_patterns in COMMON_PATTERNS.items()
    }


def get_high_confidence_patterns(language: str) -> dict[IntegrationType, list[str]]:
    """Get high confidence patterns for a specific language.

    Args:
        language: The programming language.

    Returns:
        Dict mapping IntegrationType to high confidence pattern lists.
    """
    common = HIGH_CONFIDENCE_PATTERNS.get("common", {})
    language_specific = HIGH_CONFIDENCE_PATTERNS.get(language, {})

    return {
        integration_type: common.get(integration_type, [])
        + language_specific.get(integration_type, [])
        for integration_type in IntegrationType
    }


def first_matching_pattern(text: str, patterns: list[str]) -> str | None:
    """Find the first pattern that matches the text.

    Args:
        text: The text to check.
        patterns: List of regex patterns to match against.

    Returns:
        The first matched pattern string, or None if no match.
    """
    return next(
        (pattern for pattern in patterns if re.search(pattern, text)),
        None,
    )


def _is_high_confidence_pattern(
    integration_type: IntegrationType, matched_pattern: str, language: str
) -> bool:
    """Check if the matched pattern is a high confidence pattern."""
    high_patterns = get_high_confidence_patterns(language).get(integration_type, [])
    return any(re.search(hp, matched_pattern) for hp in high_patterns)


def _has_strong_keyword(matched_pattern: str) -> bool:
    """Check if the pattern contains a strong keyword."""
    pattern_lower = matched_pattern.lower()
    return any(kw in pattern_lower for kw in STRONG_KEYWORDS)


def determine_confidence(
    integration_type: IntegrationType,
    matched_pattern: str,
    is_name_match: bool,
    is_signature_match: bool,
    is_scope_match: bool,
    language: str,
) -> Confidence:
    """Determine confidence level based on match characteristics.

    Args:
        integration_type: The type of integration detected.
        matched_pattern: The pattern that triggered the match.
        is_name_match: Whether the match was on the symbol name.
        is_signature_match: Whether the match was on the signature.
        is_scope_match: Whether the match was on the scope.
        language: The programming language.

    Returns:
        Confidence level.
    """
    if _is_high_confidence_pattern(integration_type, matched_pattern, language):
        return Confidence.HIGH

    if is_signature_match:
        return Confidence.HIGH

    if is_scope_match:
        return Confidence.MEDIUM

    if is_name_match and _has_strong_keyword(matched_pattern):
        return Confidence.MEDIUM

    return Confidence.LOW


def _check_field(
    text: str,
    patterns: list[str],
    integration_type: IntegrationType,
    field_name: str,
    is_name: bool,
    is_signature: bool,
    is_scope: bool,
    language: str,
) -> Iterator[tuple[IntegrationType, Confidence, str]]:
    """Check a single field against patterns and yield matches.

    Args:
        text: The text to check.
        patterns: Patterns to match against.
        integration_type: The integration type being checked.
        field_name: Name of the field for the matched_pattern prefix.
        is_name: Whether this is a name field match.
        is_signature: Whether this is a signature field match.
        is_scope: Whether this is a scope field match.
        language: The programming language.

    Yields:
        Tuples of (integration_type, confidence, matched_pattern).
    """
    matched = first_matching_pattern(text, patterns)
    if matched is not None:
        confidence = determine_confidence(
            integration_type, matched, is_name, is_signature, is_scope, language
        )
        yield (integration_type, confidence, f"{field_name}:{matched}")


def _classify_for_integration_type(
    entry: CTagsEntry,
    integration_type: IntegrationType,
    patterns: dict[str, list[str]],
    language: str,
) -> Iterator[tuple[IntegrationType, Confidence, str]]:
    """Classify an entry for a single integration type.

    Args:
        entry: The CTags entry to classify.
        integration_type: The integration type to check.
        patterns: The patterns dict for this integration type.
        language: The programming language.

    Yields:
        Tuples of (integration_type, confidence, matched_pattern).
    """
    yield from _check_field(
        entry.name,
        patterns.get("name_patterns", []),
        integration_type,
        "name",
        is_name=True,
        is_signature=False,
        is_scope=False,
        language=language,
    )
    if entry.signature is not None:
        yield from _check_field(
            entry.signature,
            patterns.get("signature_patterns", []),
            integration_type,
            "signature",
            is_name=False,
            is_signature=True,
            is_scope=False,
            language=language,
        )
    if entry.scope is not None:
        yield from _check_field(
            entry.scope,
            patterns.get("scope_patterns", []),
            integration_type,
            "scope",
            is_name=False,
            is_signature=False,
            is_scope=True,
            language=language,
        )


def classify_entry(entry: CTagsEntry) -> list[tuple[IntegrationType, Confidence, str]]:
    """Classify a single CTags entry into integration types.

    Uses the entry's language field to select appropriate patterns.

    Args:
        entry: The CTags entry to classify.

    Returns:
        List of (integration_type, confidence, matched_pattern) tuples.
    """
    language = entry.language or ""
    patterns_for_language = get_patterns_for_language(language)

    return list(
        chain.from_iterable(
            _classify_for_integration_type(
                entry, integration_type, patterns, language
            )
            for integration_type, patterns in patterns_for_language.items()
        )
    )


def classify_directory(
    directory_name: str,
) -> list[tuple[IntegrationType, Confidence, str]]:
    """Classify a directory name into integration types.

    Uses common directory patterns only (not language-specific).

    Args:
        directory_name: The directory name to classify.

    Returns:
        List of (integration_type, confidence, matched_pattern) tuples.
    """
    matches: list[tuple[IntegrationType, Confidence, str]] = []

    for integration_type, patterns in COMMON_PATTERNS.items():
        directory_patterns = patterns.get("directory_patterns", [])
        matched = first_matching_pattern(directory_name, directory_patterns)
        if matched is not None:
            matches.append(
                (integration_type, Confidence.MEDIUM, f"directory:{matched}")
            )

    return matches


def _extract_directories(entries: list[CTagsEntry]) -> set[str]:
    """Extract unique directory names from CTags entries.

    Args:
        entries: List of CTags entries.

    Returns:
        Set of unique directory names found in the paths.
    """
    directories: set[str] = set()

    for entry in entries:
        path = Path(entry.path)
        for parent in path.parents:
            if parent.name and parent.name not in (".", ""):
                directories.add(parent.name)

    return directories


def _entry_to_integration_points(
    entry: CTagsEntry,
) -> Iterator[IntegrationPoint]:
    """Convert a CTags entry to integration points.

    Args:
        entry: The CTags entry to process.

    Yields:
        IntegrationPoint instances for each classification match.
    """
    return (
        IntegrationPoint(
            entry=entry,
            integration_type=integration_type,
            confidence=confidence,
            matched_pattern=matched_pattern,
            entity_type=EntityType.CODE_SYMBOL,
        )
        for integration_type, confidence, matched_pattern in classify_entry(entry)
    )


def _directory_to_integration_points(
    directory_name: str,
    representative_entry: CTagsEntry,
) -> Iterator[IntegrationPoint]:
    """Convert a directory name to integration points.

    Args:
        directory_name: The directory name to check.
        representative_entry: A representative entry from this directory.

    Yields:
        IntegrationPoint instances for each classification match.
    """
    return (
        IntegrationPoint(
            entry=representative_entry,
            integration_type=integration_type,
            confidence=confidence,
            matched_pattern=matched_pattern,
            entity_type=EntityType.DIRECTORY,
        )
        for integration_type, confidence, matched_pattern in classify_directory(
            directory_name
        )
    )


def _find_representative_entry(
    directory_name: str, entries: list[CTagsEntry]
) -> CTagsEntry | None:
    """Find a representative entry for a directory.

    Args:
        directory_name: The directory name to find an entry for.
        entries: List of CTags entries to search.

    Returns:
        A representative entry from the directory, or None if not found.
    """
    for entry in entries:
        if directory_name in entry.path:
            return entry
    return None


def detect_integrations(result: CTagsResult) -> IntegrationDetectorResult:
    """Detect integration points from CTags result.

    Analyzes both code symbols and directory names for integration patterns.
    Uses language-specific patterns based on each entry's language field.

    Args:
        result: The CTags result containing code symbols.

    Returns:
        IntegrationDetectorResult with detected integration points.
    """
    # Detect from code symbols
    code_symbol_points = list(
        chain.from_iterable(
            _entry_to_integration_points(entry) for entry in result.entries
        )
    )

    # Detect from directory names
    directories = _extract_directories(result.entries)
    directory_points: list[IntegrationPoint] = []

    for directory_name in directories:
        representative = _find_representative_entry(directory_name, result.entries)
        if representative is not None:
            directory_points.extend(
                _directory_to_integration_points(directory_name, representative)
            )

    return IntegrationDetectorResult(
        integration_points=code_symbol_points + directory_points,
        entries_scanned=len(result.entries),
    )
