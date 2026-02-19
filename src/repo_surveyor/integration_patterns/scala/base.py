"""Scala base integration patterns."""

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
                (r"import sttp\.client", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import scalaj\.http", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import dispatch\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"HttpURLConnection", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"import requests\.", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"import scalaxb\.", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import javax\.xml\.ws", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"import org\.apache\.kafka",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"KafkaProducer", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"KafkaConsumer", Confidence.HIGH, SignalDirection.INWARD),
                (r"import com\.rabbitmq", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import fs2\.kafka", Confidence.HIGH, SignalDirection.AMBIGUOUS),
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
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"import slick\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import doobie\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import quill\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import scalikejdbc\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import reactivemongo\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import neotypes\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"DriverManager\.getConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"import anorm\.", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"import scala\.io\.Source",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                ),
                (
                    r"import java\.nio\.file",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"import better\.files", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import os\.", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"import io\.grpc", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import scalapb\.", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"StreamObserver", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"import sangria\.", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"import caliban\.", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"GraphQLSchema", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"import javax\.mail", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import jakarta\.mail", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"import courier\.", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"import scalacache\.", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"import redis\.clients\.jedis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"import io\.lettuce", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"import com\.github\.benmanes\.caffeine",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"text/event-stream", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"Source\.tick", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"ServerSentEvent", Confidence.HIGH, SignalDirection.INWARD),
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
                (r"import akka\.scheduler", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"system\.scheduler", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
