"""Java integration patterns."""

from .types import Confidence, IntegrationType

BASE_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
            (r"HttpServletRequest", Confidence.MEDIUM),
            (r"HttpServletResponse", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOAP: {
        "patterns": [
            (r"@WebService", Confidence.HIGH),
            (r"@WebMethod", Confidence.HIGH),
            (r"@WebParam", Confidence.HIGH),
            (r"@SOAPBinding", Confidence.HIGH),
            (r"SOAPMessage", Confidence.HIGH),
            (r"SOAPEnvelope", Confidence.HIGH),
            (r"JAXBContext", Confidence.MEDIUM),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"MessageListener", Confidence.MEDIUM),
            (r"MessageProducer", Confidence.MEDIUM),
            (r"MessageConsumer", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"@ServerEndpoint", Confidence.HIGH),
            (r"@OnOpen", Confidence.HIGH),
            (r"@OnClose", Confidence.HIGH),
            (r"@OnMessage", Confidence.HIGH),
            (r"@OnError", Confidence.HIGH),
            (r"ServerSocket\b", Confidence.HIGH),
            (r"DatagramSocket", Confidence.HIGH),
            (r"SocketChannel", Confidence.MEDIUM),
        ],
    },
    IntegrationType.DATABASE: {
        "patterns": [
            (r"@Entity", Confidence.HIGH),
            (r"@Table", Confidence.HIGH),
            (r"@Column", Confidence.MEDIUM),
            (r"@JoinColumn", Confidence.MEDIUM),
            (r"@OneToMany", Confidence.MEDIUM),
            (r"@ManyToOne", Confidence.MEDIUM),
            (r"@ManyToMany", Confidence.MEDIUM),
            (r"@Transactional", Confidence.HIGH),
            (r"@PersistenceContext", Confidence.HIGH),
            (r"EntityManager", Confidence.HIGH),
            (r"PreparedStatement", Confidence.HIGH),
            (r"ResultSet\b", Confidence.MEDIUM),
        ],
    },
}

FRAMEWORK_PATTERNS = {
    "Spring": {
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
    "JAX-RS": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"@GET\b", Confidence.HIGH),
                (r"@POST\b", Confidence.HIGH),
                (r"@PUT\b", Confidence.HIGH),
                (r"@DELETE\b", Confidence.HIGH),
                (r"@Path\(", Confidence.HIGH),
                (r"@Produces", Confidence.HIGH),
                (r"@Consumes", Confidence.HIGH),
            ],
        },
    },
    "Micronaut": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"import io\.micronaut", Confidence.HIGH),
                (r"@Client\(", Confidence.HIGH),
            ],
        },
    },
    "Quarkus": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"import io\.quarkus", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            "patterns": [
                (r"@Incoming\(", Confidence.HIGH),
                (r"@Outgoing\(", Confidence.HIGH),
            ],
        },
    },
}
