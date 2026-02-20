"""Spring framework integration patterns for Kotlin."""

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
                (r"@RestController", Confidence.HIGH, SignalDirection.INWARD),
                (r"@GetMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PostMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@PutMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@DeleteMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"@RequestMapping", Confidence.HIGH, SignalDirection.INWARD),
                (r"coRouter\s*\{", Confidence.HIGH, SignalDirection.INWARD),
                (r"router\s*\{", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"@KafkaListener", Confidence.HIGH, SignalDirection.INWARD),
                (r"@JmsListener", Confidence.HIGH, SignalDirection.INWARD),
                (r"@RabbitListener", Confidence.HIGH, SignalDirection.INWARD),
                (r"KafkaTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"@Repository", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Query", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"JdbcTemplate", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"R2dbcRepository", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"@Cacheable", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@CacheEvict", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@EnableCaching", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"@Scheduled", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@EnableScheduling", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
