"""Spring framework integration patterns."""

from ..types import Confidence, FrameworkPatternSpec, IntegrationType

FRAMEWORK = FrameworkPatternSpec(
    name="Spring",
    patterns={
        IntegrationType.HTTP_REST: {
            "patterns": [
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
            "patterns": [
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
            "patterns": [
                (r"@Repository", Confidence.HIGH),
                (r"@Query", Confidence.HIGH),
                (r"JdbcTemplate", Confidence.HIGH),
                (r"SessionFactory", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            "patterns": [
                (r"WebSocketHandler", Confidence.HIGH),
            ],
        },
    },
)
