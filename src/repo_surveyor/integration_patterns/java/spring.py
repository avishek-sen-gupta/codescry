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
                        "Spring @RestController annotation defining a REST API controller",
                        "This code uses Spring MVC to expose an inbound REST API controller",
                    ),
                ),
                (
                    r"@Controller",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @Controller annotation defining an MVC web controller",
                        "This code uses Spring MVC to expose an inbound HTTP endpoint",
                    ),
                ),
                (
                    r"@RequestMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @RequestMapping annotation mapping HTTP requests to handler methods",
                        "This code uses Spring MVC to handle incoming HTTP requests",
                    ),
                ),
                (
                    r"@GetMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @GetMapping annotation defining an HTTP GET endpoint handler",
                        "This code uses Spring MVC to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"@PostMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @PostMapping annotation defining an HTTP POST endpoint handler",
                        "This code uses Spring MVC to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"@PutMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @PutMapping annotation defining an HTTP PUT endpoint handler",
                        "This code uses Spring MVC to handle incoming HTTP PUT requests",
                    ),
                ),
                (
                    r"@DeleteMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @DeleteMapping annotation defining an HTTP DELETE endpoint handler",
                        "This code uses Spring MVC to handle incoming HTTP DELETE requests",
                    ),
                ),
                (
                    r"@PatchMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @PatchMapping annotation defining an HTTP PATCH endpoint handler",
                        "This code uses Spring MVC to handle incoming HTTP PATCH requests",
                    ),
                ),
                (
                    r"@RequestBody",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @RequestBody annotation binding an HTTP request body to a method parameter",
                        "This code uses Spring MVC to receive inbound HTTP request body data",
                    ),
                ),
                (
                    r"@ResponseBody",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @ResponseBody annotation serializing return values to the HTTP response body",
                        "This code uses Spring MVC to handle inbound HTTP requests with response serialization",
                    ),
                ),
                (
                    r"@PathVariable",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @PathVariable annotation extracting values from URI path segments",
                        "This code uses Spring MVC to handle inbound HTTP requests with path variables",
                    ),
                ),
                (
                    r"@RequestParam",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @RequestParam annotation binding HTTP query parameters to method parameters",
                        "This code uses Spring MVC to handle inbound HTTP requests with query parameters",
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
                        "Spring @KafkaListener annotation consuming Kafka messages",
                        "This code uses Spring Kafka to consume incoming messages from a Kafka topic",
                    ),
                ),
                (
                    r"@JmsListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @JmsListener annotation consuming JMS messages from a queue or topic",
                        "This code uses Spring JMS to listen for incoming messages from a JMS queue",
                    ),
                ),
                (
                    r"@RabbitListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @RabbitListener annotation consuming AMQP messages from a RabbitMQ queue",
                        "This code uses Spring AMQP to listen for incoming messages from a RabbitMQ queue",
                    ),
                ),
                (
                    r"@StreamListener",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @StreamListener annotation consuming messages from a Spring Cloud Stream binding",
                        "This code uses Spring Cloud Stream to listen for incoming messages from a message queue",
                    ),
                ),
                (
                    r"@SendTo",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring @SendTo annotation forwarding method return values to a messaging destination",
                        "This code uses Spring Messaging to send outgoing messages to a message queue",
                    ),
                ),
                (
                    r"KafkaTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring KafkaTemplate for producing Kafka messages",
                        "This code uses Spring Kafka to send outgoing messages to a Kafka topic",
                    ),
                ),
                (
                    r"JmsTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring JmsTemplate for sending JMS messages to a queue or topic",
                        "This code uses Spring JMS to send outgoing messages to a JMS queue",
                    ),
                ),
                (
                    r"RabbitTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring RabbitTemplate for publishing AMQP messages to a RabbitMQ exchange",
                        "This code uses Spring AMQP to send outgoing messages to a RabbitMQ queue",
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
                        "Spring @Repository annotation marking a class as a data access component",
                        "This code uses Spring Data to query a database",
                    ),
                ),
                (
                    r"@Query",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring @Query annotation declaring a custom database query on a repository method",
                        "This code uses Spring Data to execute a custom query against a database",
                    ),
                ),
                (
                    r"JdbcTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring JdbcTemplate for executing SQL queries against a relational database",
                        "This code uses Spring JDBC to query a relational database",
                    ),
                ),
                (
                    r"SessionFactory",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Hibernate SessionFactory for managing database sessions",
                        "This code uses Spring Hibernate to query a relational database",
                    ),
                ),
                (
                    r"@Node",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Data Neo4j @Node annotation mapping a class to a graph node",
                        "This code uses Spring Data Neo4j to write a graph node to a database",
                    ),
                ),
                (
                    r"@Relationship",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Data Neo4j @Relationship annotation mapping a class to a graph relationship",
                        "This code uses Spring Data Neo4j to write a graph relationship to a database",
                    ),
                ),
                (
                    r"Neo4jRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Data Neo4j Neo4jRepository interface for graph database access",
                        "This code uses Spring Data Neo4j to query a Neo4j graph database",
                    ),
                ),
                (
                    r"ReactiveNeo4jRepository",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Data Neo4j ReactiveNeo4jRepository for reactive graph database access",
                        "This code uses Spring Data Neo4j to reactively query a Neo4j graph database",
                    ),
                ),
                (
                    r"Neo4jTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Data Neo4j Neo4jTemplate for executing graph database operations",
                        "This code uses Spring Data Neo4j to query a Neo4j graph database",
                    ),
                ),
                (
                    r"@EnableNeo4jRepositories",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Data Neo4j @EnableNeo4jRepositories annotation activating Neo4j repository support",
                        "This code uses Spring Data Neo4j to enable querying a Neo4j graph database",
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
                        "Spring WebSocketHandler interface for handling WebSocket connections",
                        "This code uses Spring WebSocket to handle inbound WebSocket connections",
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
                        "Spring @QueryMapping annotation handling GraphQL query operations",
                        "This code uses Spring GraphQL to handle inbound GraphQL query requests",
                    ),
                ),
                (
                    r"@MutationMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @MutationMapping annotation handling GraphQL mutation operations",
                        "This code uses Spring GraphQL to handle inbound GraphQL mutation requests",
                    ),
                ),
                (
                    r"@SchemaMapping",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Spring @SchemaMapping annotation mapping a handler method to a GraphQL schema field",
                        "This code uses Spring GraphQL to handle inbound GraphQL schema field requests",
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
                        "Spring SimpleMailMessage class for composing and sending plain text emails",
                        "This code uses Spring Mail to send outgoing email messages",
                    ),
                ),
                (
                    r"MimeMessageHelper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring MimeMessageHelper for composing and sending MIME email messages",
                        "This code uses Spring Mail to send outgoing email messages via SMTP server",
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
                        "Spring @Cacheable annotation caching the result of a method",
                        "This code uses Spring Cache to interact with a cache store",
                    ),
                ),
                (
                    r"@CacheEvict",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Spring @CacheEvict annotation evicting entries from a cache",
                        "This code uses Spring Cache to interact with a cache store",
                    ),
                ),
                (
                    r"@CachePut",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Spring @CachePut annotation updating a cache entry with a method result",
                        "This code uses Spring Cache to interact with a cache store",
                    ),
                ),
                (
                    r"RedisTemplate",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring RedisTemplate for executing Redis commands against a Redis cache",
                        "This code uses Spring Data Redis to connect to a Redis cache store",
                    ),
                ),
                (
                    r"@EnableCaching",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Spring @EnableCaching annotation activating Spring's annotation-driven cache management",
                        "This code uses Spring Cache to work with a cache store",
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
                        "Spring Flux<ServerSentEvent> return type for streaming server-sent events to clients",
                        "This code uses Spring WebFlux to expose an inbound server-sent event stream",
                    ),
                ),
                (
                    r"@Tailable",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Spring @Tailable annotation enabling tailable cursor queries on capped MongoDB collections",
                        "This code uses Spring Data to interact with a server-sent event stream",
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
                        "Spring Integration FtpInboundFileSynchronizer for downloading files from an FTP server",
                        "This code uses Spring Integration FTP to connect to an FTP server",
                    ),
                ),
                (
                    r"SftpInboundFileSynchronizer",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Integration SftpInboundFileSynchronizer for downloading files from an SFTP server",
                        "This code uses Spring Integration SFTP to connect to an SFTP connection",
                    ),
                ),
                (
                    r"@ServiceActivator.*ftp",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Spring Integration @ServiceActivator wired to an FTP channel adapter",
                        "This code uses Spring Integration to connect to an FTP server",
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
                        "Spring @Scheduled annotation triggering a method on a fixed schedule",
                        "This code uses Spring Scheduling to work with a scheduled task",
                    ),
                ),
                (
                    r"@EnableScheduling",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Spring @EnableScheduling annotation activating Spring's scheduled task execution",
                        "This code uses Spring Scheduling to work with scheduled tasks",
                    ),
                ),
                (
                    r"TaskScheduler",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Spring TaskScheduler interface for programmatically scheduling tasks",
                        "This code uses Spring Scheduling to work with a scheduled task",
                    ),
                ),
            ],
        },
    },
)
