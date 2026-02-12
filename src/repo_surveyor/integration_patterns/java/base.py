"""Java base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"HttpServletRequest", Confidence.MEDIUM),
                (r"HttpServletResponse", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
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
            PatternKey.PATTERNS: [
                (r"MessageListener", Confidence.MEDIUM),
                (r"MessageProducer", Confidence.MEDIUM),
                (r"MessageConsumer", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
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
            PatternKey.PATTERNS: [
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
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"FileInputStream", Confidence.HIGH),
                (r"FileOutputStream", Confidence.HIGH),
                (r"BufferedReader", Confidence.MEDIUM),
                (r"Files\.read", Confidence.HIGH),
                (r"Files\.write", Confidence.HIGH),
                (r"Files\.copy", Confidence.HIGH),
                (r"AmazonS3Client", Confidence.HIGH),
                (r"FTPClient", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"import io\.grpc", Confidence.HIGH),
                (r"@GrpcService", Confidence.HIGH),
                (r"StreamObserver", Confidence.HIGH),
                (r"ManagedChannel", Confidence.HIGH),
            ],
        },
    },
)
