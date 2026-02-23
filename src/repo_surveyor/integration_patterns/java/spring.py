"""Spring framework integration patterns."""

from repo_surveyor.integration_patterns.types import (
    Confidence,
    FrameworkPatternSpec,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

FRAMEWORK = FrameworkPatternSpec(
    name="Spring",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r"@RestController",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "REST controller is annotated by @RestController",
                        "REST controller is exposed for inbound API",
                    ),
                ),
                (
                    r"@Controller",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "MVC controller is annotated by @Controller",
                        "HTTP endpoint is exposed for inbound requests",
                    ),
                ),
                (
                    r"@RequestMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP requests are mapped by @RequestMapping annotation",
                        "HTTP requests are handled",
                    ),
                ),
                (
                    r"@GetMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP GET endpoint is annotated for handling",
                        "HTTP GET requests are handled",
                    ),
                ),
                (
                    r"@PostMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP POST endpoint is annotated for handling",
                        "HTTP POST requests are handled",
                    ),
                ),
                (
                    r"@PutMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP PUT endpoint is annotated for handling",
                        "HTTP PUT requests are handled",
                    ),
                ),
                (
                    r"@DeleteMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP DELETE endpoint is annotated for handling",
                        "HTTP DELETE requests are handled",
                    ),
                ),
                (
                    r"@PatchMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP PATCH endpoint is annotated for handling",
                        "HTTP PATCH requests are handled",
                    ),
                ),
                (
                    r"@RequestBody",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP request body is bound by @RequestBody annotation",
                        "HTTP request body is received",
                    ),
                ),
                (
                    r"@ResponseBody",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Response body is serialized by @ResponseBody annotation",
                        "HTTP requests are handled with response serialization",
                    ),
                ),
                (
                    r"@PathVariable",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "URI path values are extracted by @PathVariable annotation",
                        "HTTP requests are handled with path variables",
                    ),
                ),
                (
                    r"@RequestParam",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Query parameters are bound by @RequestParam annotation",
                        "HTTP requests are handled with query parameters",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"@KafkaListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Kafka messages are consumed by @KafkaListener annotation",
                        "Kafka messages are consumed from topic",
                    ),
                ),
                (
                    r"@JmsListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "JMS messages are consumed by @JmsListener annotation",
                        "JMS messages are consumed from queue",
                    ),
                ),
                (
                    r"@RabbitListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "AMQP messages are consumed by @RabbitListener annotation",
                        "Messages are listened from RabbitMQ with Spring AMQP",
                    ),
                ),
                (
                    r"@StreamListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Stream messages are consumed by @StreamListener annotation",
                        "Messages are listened from message queue",
                    ),
                ),
                (
                    r"@SendTo",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Return values are forwarded by @SendTo annotation",
                        "Messages are sent to message queue",
                    ),
                ),
                (
                    r"KafkaTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Kafka messages are produced via template",
                        "Kafka messages are sent to topic",
                    ),
                ),
                (
                    r"JmsTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "JMS messages are sent to queue via template",
                        "JMS messages are sent to queue",
                    ),
                ),
                (
                    r"RabbitTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "AMQP messages are published to RabbitMQ exchange",
                        "Messages are sent to RabbitMQ with Spring AMQP",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"@Repository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Data access component is marked by @Repository annotation",
                        "Database is queried with Spring Data",
                    ),
                ),
                (
                    r"@Query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database query is declared by @Query annotation",
                        "Database is queried with custom Spring Data query",
                    ),
                ),
                (
                    r"JdbcTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL queries are executed against database via JdbcTemplate",
                        "Database is queried with JDBC",
                    ),
                ),
                (
                    r"SessionFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database sessions are managed by Hibernate SessionFactory",
                        "Database is queried with Hibernate ORM",
                    ),
                ),
                (
                    r"@Node",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph node is mapped by @Node annotation",
                        "Graph node is written to database",
                    ),
                ),
                (
                    r"@Relationship",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph relationship is mapped by @Relationship annotation",
                        "Graph relationship is written to database",
                    ),
                ),
                (
                    r"Neo4jRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database repository is accessed with Neo4j interface",
                        "Neo4j graph database is queried with Spring Data",
                    ),
                ),
                (
                    r"ReactiveNeo4jRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Reactive graph database is accessed with Neo4j repository",
                        "Neo4j graph database is reactively queried",
                    ),
                ),
                (
                    r"Neo4jTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database operations are executed via Neo4j template",
                        "Neo4j graph database is queried with Spring Data",
                    ),
                ),
                (
                    r"@EnableNeo4jRepositories",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j repositories are activated by @EnableNeo4jRepositories annotation",
                        "Neo4j graph database is queried with Spring Data",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"WebSocketHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WebSocket connections are handled by interface",
                        "WebSocket connections are handled inbound",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"@QueryMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL queries are handled by @QueryMapping annotation",
                        "GraphQL query requests are handled with Spring",
                    ),
                ),
                (
                    r"@MutationMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL mutations are handled by @MutationMapping annotation",
                        "GraphQL mutation requests are handled with Spring",
                    ),
                ),
                (
                    r"@SchemaMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL schema field is mapped by @SchemaMapping annotation",
                        "GraphQL schema field is handled for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"SimpleMailMessage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Plain text emails are composed and sent",
                        "Email messages are sent via SMTP",
                    ),
                ),
                (
                    r"MimeMessageHelper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MIME email messages are composed and sent",
                        "Email messages are sent via SMTP server",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"@Cacheable",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Method result is cached by @Cacheable annotation",
                        "Cache store is interacted with Spring Cache",
                    ),
                ),
                (
                    r"@CacheEvict",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache entries are evicted by @CacheEvict annotation",
                        "Cache store is interacted with Spring Cache",
                    ),
                ),
                (
                    r"@CachePut",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache entry is updated by @CachePut annotation",
                        "Cache store is interacted with Spring Cache",
                    ),
                ),
                (
                    r"RedisTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis commands are executed against cache via template",
                        "Redis cache store is connected with Spring Data",
                    ),
                ),
                (
                    r"@EnableCaching",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Cache management is activated by @EnableCaching annotation",
                        "Cache store is worked with Spring Cache",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"Flux<ServerSentEvent",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Server-sent events are streamed to clients via Flux",
                        "Server-sent events are streamed inbound",
                    ),
                ),
                (
                    r"@Tailable",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Tailable cursor queries are enabled by @Tailable annotation",
                        "Server-sent event stream is interacted with",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"FtpInboundFileSynchronizer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP files are downloaded by inbound synchronizer",
                        "FTP server is connected via Integration",
                    ),
                ),
                (
                    r"SftpInboundFileSynchronizer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP files are downloaded by inbound synchronizer",
                        "SFTP connection is established via Integration",
                    ),
                ),
                (
                    r"@ServiceActivator.*ftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP channel adapter is wired with ServiceActivator",
                        "FTP server is connected via Integration",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"@Scheduled",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Method is scheduled by @Scheduled annotation",
                        "Scheduled task is executed",
                    ),
                ),
                (
                    r"@EnableScheduling",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Scheduled tasks are activated by @EnableScheduling annotation",
                        "Scheduled tasks are executed",
                    ),
                ),
                (
                    r"TaskScheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Tasks are scheduled programmatically via interface",
                        "Scheduled task is executed",
                    ),
                ),
            ],
        },
    },
)
