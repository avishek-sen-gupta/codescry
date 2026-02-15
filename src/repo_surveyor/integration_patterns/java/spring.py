"""Spring framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Spring",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@RestController", Confidence.HIGH),
                (r"@Controller", Confidence.HIGH),
                (r"@RequestMapping", Confidence.HIGH),
                (r"@GetMapping", Confidence.HIGH),
                (r"@PostMapping", Confidence.HIGH),
                (r"@PutMapping", Confidence.HIGH),
                (r"@DeleteMapping", Confidence.HIGH),
                (r"@PatchMapping", Confidence.HIGH),
                (r"@RequestBody", Confidence.HIGH),
                (r"@ResponseBody", Confidence.HIGH),
                (r"@PathVariable", Confidence.HIGH),
                (r"@RequestParam", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@KafkaListener", Confidence.HIGH),
                (r"@JmsListener", Confidence.HIGH),
                (r"@RabbitListener", Confidence.HIGH),
                (r"@StreamListener", Confidence.HIGH),
                (r"@SendTo", Confidence.HIGH),
                (r"KafkaTemplate", Confidence.HIGH),
                (r"JmsTemplate", Confidence.HIGH),
                (r"RabbitTemplate", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"@Repository", Confidence.HIGH),
                (r"@Query", Confidence.HIGH),
                (r"JdbcTemplate", Confidence.HIGH),
                (r"SessionFactory", Confidence.HIGH),
                (r"@Node", Confidence.HIGH),
                (r"@Relationship", Confidence.HIGH),
                (r"Neo4jRepository", Confidence.HIGH),
                (r"ReactiveNeo4jRepository", Confidence.HIGH),
                (r"Neo4jTemplate", Confidence.HIGH),
                (r"@EnableNeo4jRepositories", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"WebSocketHandler", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"@QueryMapping", Confidence.HIGH),
                (r"@MutationMapping", Confidence.HIGH),
                (r"@SchemaMapping", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"SimpleMailMessage", Confidence.HIGH),
                (r"MimeMessageHelper", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"@Cacheable", Confidence.HIGH),
                (r"@CacheEvict", Confidence.HIGH),
                (r"@CachePut", Confidence.HIGH),
                (r"RedisTemplate", Confidence.HIGH),
                (r"@EnableCaching", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"Flux<ServerSentEvent", Confidence.HIGH),
                (r"@Tailable", Confidence.HIGH),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"FtpInboundFileSynchronizer", Confidence.HIGH),
                (r"SftpInboundFileSynchronizer", Confidence.HIGH),
                (r"@ServiceActivator.*ftp", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"@Scheduled", Confidence.HIGH),
                (r"@EnableScheduling", Confidence.HIGH),
                (r"TaskScheduler", Confidence.HIGH),
            ],
        },
    },
)
