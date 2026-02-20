"""Java base integration patterns."""

from repo_surveyor.integration_patterns.types import (
    BasePatternSpec,
    Confidence,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"HttpServletRequest", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"HttpServletResponse", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"@WebService", Confidence.HIGH, SignalDirection.INWARD),
                (r"@WebMethod", Confidence.HIGH, SignalDirection.INWARD),
                (r"@WebParam", Confidence.HIGH, SignalDirection.INWARD),
                (r"@SOAPBinding", Confidence.HIGH, SignalDirection.INWARD),
                (r"SOAPMessage", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"SOAPEnvelope", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"JAXBContext", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"MessageListener", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"MessageProducer", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"MessageConsumer", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"SqsClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SnsClient", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"@ServerEndpoint", Confidence.HIGH, SignalDirection.INWARD),
                (r"@OnOpen", Confidence.HIGH, SignalDirection.INWARD),
                (r"@OnClose", Confidence.HIGH, SignalDirection.INWARD),
                (r"@OnMessage", Confidence.HIGH, SignalDirection.INWARD),
                (r"@OnError", Confidence.HIGH, SignalDirection.INWARD),
                (r"ServerSocket\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"DatagramSocket", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"SocketChannel", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"@Entity", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Table", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Column", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@JoinColumn", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@OneToMany", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@ManyToOne", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@ManyToMany", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@Transactional", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@PersistenceContext", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EntityManager", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PreparedStatement", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ResultSet\b", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"DynamoDbClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"import org\.neo4j\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"GraphDatabase\.driver", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"DriverManager\.getConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"FileInputStream", Confidence.HIGH, SignalDirection.INWARD),
                (r"FileOutputStream", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"BufferedReader", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"Files\.read", Confidence.HIGH, SignalDirection.INWARD),
                (r"Files\.write", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Files\.copy", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"AmazonS3Client", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"import com\.azure\.storage\.blob",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"import com\.google\.cloud\.storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"import io\.grpc", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@GrpcService", Confidence.HIGH, SignalDirection.INWARD),
                (r"StreamObserver", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"ManagedChannel", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"import graphql\.", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"GraphQLSchema", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"javax\.mail", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"jakarta\.mail", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"import redis\.clients\.jedis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"import io\.lettuce", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import javax\.cache", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"SseEmitter", Confidence.HIGH, SignalDirection.INWARD),
                (r"text/event-stream", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"Flux<", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"FTPClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"JSch", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ChannelSftp", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SFTPClient", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"import org\.quartz", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@Schedule", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"ScheduledExecutorService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
    },
)
