"""Scala base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"import sttp\.client", Confidence.HIGH),
                (r"import scalaj\.http", Confidence.HIGH),
                (r"import dispatch\.", Confidence.HIGH),
                (r"HttpURLConnection", Confidence.MEDIUM),
                (r"import requests\.", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"import scalaxb\.", Confidence.HIGH),
                (r"import javax\.xml\.ws", Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"import org\.apache\.kafka", Confidence.HIGH),
                (r"KafkaProducer", Confidence.HIGH),
                (r"KafkaConsumer", Confidence.HIGH),
                (r"import com\.rabbitmq", Confidence.HIGH),
                (r"import fs2\.kafka", Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"ServerSocket\(", Confidence.HIGH),
                (r"DatagramSocket\(", Confidence.HIGH),
                (r"import java\.nio\.channels", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import slick\.", Confidence.HIGH),
                (r"import doobie\.", Confidence.HIGH),
                (r"import quill\.", Confidence.HIGH),
                (r"import scalikejdbc\.", Confidence.HIGH),
                (r"import reactivemongo\.", Confidence.HIGH),
                (r"import neotypes\.", Confidence.HIGH),
                (r"DriverManager\.getConnection", Confidence.HIGH),
                (r"import anorm\.", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"import scala\.io\.Source", Confidence.MEDIUM),
                (r"import java\.nio\.file", Confidence.MEDIUM),
                (r"import better\.files", Confidence.HIGH),
                (r"import os\.", Confidence.MEDIUM),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"import io\.grpc", Confidence.HIGH),
                (r"import scalapb\.", Confidence.HIGH),
                (r"StreamObserver", Confidence.HIGH),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"import sangria\.", Confidence.HIGH),
                (r"import caliban\.", Confidence.HIGH),
                (r"GraphQLSchema", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"import javax\.mail", Confidence.HIGH),
                (r"import jakarta\.mail", Confidence.HIGH),
                (r"import courier\.", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"import scalacache\.", Confidence.HIGH),
                (r"import redis\.clients\.jedis", Confidence.HIGH),
                (r"import io\.lettuce", Confidence.HIGH),
                (r"import com\.github\.benmanes\.caffeine", Confidence.HIGH),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"text/event-stream", Confidence.MEDIUM),
                (r"Source\.tick", Confidence.MEDIUM),
                (r"ServerSentEvent", Confidence.HIGH),
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
                (r"import akka\.scheduler", Confidence.HIGH),
                (r"system\.scheduler", Confidence.MEDIUM),
            ],
        },
    },
)
