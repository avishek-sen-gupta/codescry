"""Spring framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey, SignalDirection

FRAMEWORK = FrameworkPatternSpec(
    name="Spring",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@RestController", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Controller", Confidence.HIGH, SignalDirection.INWARD),
                (r"@RequestMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@GetMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PostMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PutMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@DeleteMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PatchMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@RequestBody", Confidence.HIGH, SignalDirection.INWARD),
                (r"@ResponseBody", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PathVariable", Confidence.HIGH, SignalDirection.INWARD),
                (r"@RequestParam", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@KafkaListener", Confidence.HIGH, SignalDirection.INWARD),
                (r"@JmsListener", Confidence.HIGH, SignalDirection.INWARD),
                (r"@RabbitListener", Confidence.HIGH, SignalDirection.INWARD),
                (r"@StreamListener", Confidence.HIGH, SignalDirection.INWARD),
                (r"@SendTo", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"KafkaTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"JmsTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"RabbitTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"@Repository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Query", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"JdbcTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SessionFactory", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Node", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Relationship", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Neo4jRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ReactiveNeo4jRepository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Neo4jTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@EnableNeo4jRepositories", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"WebSocketHandler", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"@QueryMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@MutationMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@SchemaMapping", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"SimpleMailMessage", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MimeMessageHelper", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"@Cacheable", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@CacheEvict", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@CachePut", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"RedisTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@EnableCaching", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"Flux<ServerSentEvent", Confidence.HIGH, SignalDirection.INWARD),
                (r"@Tailable", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"FtpInboundFileSynchronizer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SftpInboundFileSynchronizer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@ServiceActivator.*ftp", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"@Scheduled", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@EnableScheduling", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"TaskScheduler", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
