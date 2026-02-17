"""Kotlin base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import io\.ktor\.client", Confidence.HIGH),
                (r"import okhttp3\.", Confidence.HIGH),
                (r"OkHttpClient\(", Confidence.HIGH),
                (r"import retrofit2\.", Confidence.HIGH),
                (r"HttpURLConnection", Confidence.MEDIUM),
                (r"import java\.net\.http\.HttpClient", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"@WebService", Confidence.HIGH),
                (r"@WebMethod", Confidence.HIGH),
                (r"SOAPMessage", Confidence.HIGH),
                (r"JAXBContext", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"import org\.apache\.kafka", Confidence.HIGH),
                (r"KafkaProducer\(", Confidence.HIGH),
                (r"KafkaConsumer\(", Confidence.HIGH),
                (r"import com\.rabbitmq", Confidence.HIGH),
                (r"SqsClient", Confidence.HIGH),
                (r"SnsClient", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"ServerSocket\(", Confidence.HIGH),
                (r"DatagramSocket\(", Confidence.HIGH),
                (r"import java\.nio\.channels", Confidence.MEDIUM),
                (r"@ServerEndpoint", Confidence.HIGH),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import org\.jetbrains\.exposed", Confidence.HIGH),
                (r"Database\.connect\(", Confidence.HIGH),
                (r"transaction\s*\{", Confidence.MEDIUM),
                (r"@Entity", Confidence.HIGH),
                (r"@Table", Confidence.HIGH),
                (r"@Transactional", Confidence.HIGH),
                (r"EntityManager", Confidence.HIGH),
                (r"PreparedStatement", Confidence.HIGH),
                (r"import org\.neo4j\.driver", Confidence.HIGH),
                (r"GraphDatabase\.driver", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"File\(.*\)\.readText\(", Confidence.MEDIUM),
                (r"File\(.*\)\.writeText\(", Confidence.MEDIUM),
                (r"import java\.nio\.file\.Files", Confidence.HIGH),
                (r"AmazonS3Client", Confidence.HIGH),
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
                (r"import com\.expediagroup\.graphql", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"import javax\.mail", Confidence.HIGH),
                (r"import jakarta\.mail", Confidence.HIGH),
                (r"MimeMessage\(", Confidence.HIGH),
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
                (r"FTPClient\(", Confidence.HIGH),
                (r"JSch\(", Confidence.HIGH),
                (r"ChannelSftp", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"import org\.quartz", Confidence.HIGH),
                (r"@Scheduled", Confidence.HIGH),
                (r"ScheduledExecutorService", Confidence.HIGH),
                (r"import kotlinx\.coroutines\.delay", Confidence.MEDIUM),
            ],
        },
    },
)
