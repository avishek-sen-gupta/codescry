"""Spring framework integration patterns for Kotlin."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType, PatternKey

FRAMEWORK = FrameworkPatternSpec(
    name="Spring",
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"@RestController", Confidence.HIGH),
                (r"@GetMapping", Confidence.HIGH),
                (r"@PostMapping", Confidence.HIGH),
                (r"@PutMapping", Confidence.HIGH),
                (r"@DeleteMapping", Confidence.HIGH),
                (r"@RequestMapping", Confidence.HIGH),
                (r"coRouter\s*\{", Confidence.HIGH),
                (r"router\s*\{", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@KafkaListener", Confidence.HIGH),
                (r"@JmsListener", Confidence.HIGH),
                (r"@RabbitListener", Confidence.HIGH),
                (r"KafkaTemplate", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"@Repository", Confidence.HIGH),
                (r"@Query", Confidence.HIGH),
                (r"JdbcTemplate", Confidence.HIGH),
                (r"R2dbcRepository", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"@Cacheable", Confidence.HIGH),
                (r"@CacheEvict", Confidence.HIGH),
                (r"@EnableCaching", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"@Scheduled", Confidence.HIGH),
                (r"@EnableScheduling", Confidence.HIGH),
            ],
        },
    },
)
