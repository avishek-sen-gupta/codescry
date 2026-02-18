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
                (r"SqsClient", Confidence.HIGH),
                (r"SnsClient", Confidence.HIGH),
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
                (r"DynamoDbClient", Confidence.HIGH),
                (r"import org\.neo4j\.driver", Confidence.HIGH),
                (r"GraphDatabase\.driver", Confidence.HIGH),
                (r"DriverManager\.getConnection", Confidence.HIGH),
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
                (r"import com\.azure\.storage\.blob", Confidence.HIGH),
                (r"import com\.google\.cloud\.storage", Confidence.HIGH),
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
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"import graphql\.", Confidence.HIGH),
                (r"GraphQLSchema", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"javax\.mail", Confidence.HIGH),
                (r"jakarta\.mail", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"import redis\.clients\.jedis", Confidence.HIGH),
                (r"import io\.lettuce", Confidence.HIGH),
                (r"import javax\.cache", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"SseEmitter", Confidence.HIGH),
                (r"text/event-stream", Confidence.MEDIUM),
                (r"Flux<", Confidence.MEDIUM),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"FTPClient", Confidence.HIGH),
                (r"JSch", Confidence.HIGH),
                (r"ChannelSftp", Confidence.HIGH),
                (r"SFTPClient", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"import org\.quartz", Confidence.HIGH),
                (r"@Schedule", Confidence.HIGH),
                (r"ScheduledExecutorService", Confidence.HIGH),
            ],
        },
    },
)
