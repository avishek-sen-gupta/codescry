"""Kotlin base integration patterns."""

from ..types import (
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
                (r"import io\.ktor\.client", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import okhttp3\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"OkHttpClient\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import retrofit2\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"HttpURLConnection", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (
                    r"import java\.net\.http\.HttpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"@WebService", Confidence.HIGH, SignalDirection.INWARD),
                (r"@WebMethod", Confidence.HIGH, SignalDirection.INWARD),
                (r"SOAPMessage", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"JAXBContext", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.apache\.kafka",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"KafkaProducer\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"KafkaConsumer\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"import com\.rabbitmq", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"SqsClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SnsClient", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"ServerSocket\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"DatagramSocket\(", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"import java\.nio\.channels",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"@ServerEndpoint", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.jetbrains\.exposed",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"Database\.connect\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"transaction\s*\{", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"@Entity", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Table", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"@Transactional", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"EntityManager", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PreparedStatement", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"import org\.neo4j\.driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"GraphDatabase\.driver", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"File\(.*\)\.readText\(", Confidence.MEDIUM, SignalDirection.INWARD),
                (
                    r"File\(.*\)\.writeText\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                ),
                (
                    r"import java\.nio\.file\.Files",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"AmazonS3Client", Confidence.HIGH, SignalDirection.OUTWARD),
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
                (
                    r"import com\.expediagroup\.graphql",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"import javax\.mail", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import jakarta\.mail", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MimeMessage\(", Confidence.HIGH, SignalDirection.OUTWARD),
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
                (r"FTPClient\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"JSch\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ChannelSftp", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"import org\.quartz", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"@Scheduled", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"ScheduledExecutorService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"import kotlinx\.coroutines\.delay",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
    },
)
