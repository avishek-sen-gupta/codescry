"""C/C++ base integration patterns."""

from ..types import (
    BasePatternSpec,
    Confidence,
    IntegrationType,
    PatternKey,
    SignalDirection,
)

BASE = BasePatternSpec(
    patterns={
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]sqlite3\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"sqlite3_open\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"sqlite3_exec\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"sqlite3_prepare\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'#include\s*[<"]libpq-fe\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"PQconnectdb\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"PQexec\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'#include\s*[<"]mysql\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"mysql_real_connect\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"mysql_query\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'#include\s*[<"]mongoc\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r'#include\s*[<"]sql\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (
                    r'#include\s*[<"]sqlext\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r'#include\s*[<"]leveldb/', Confidence.HIGH, SignalDirection.OUTWARD),
                (r'#include\s*[<"]rocksdb/', Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'#include\s*[<"]lmdb\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]curl/curl\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"curl_easy_init\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"curl_easy_perform\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"curl_easy_setopt\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'#include\s*[<"]cpprest/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r'#include\s*[<"]microhttpd\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]librdkafka/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"rd_kafka_new\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r'#include\s*[<"]amqp\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"amqp_new_connection\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r'#include\s*[<"]zmq\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"zmq_socket\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r'#include\s*[<"]nng/', Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]sys/socket\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r'#include\s*[<"]netinet/in\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r'#include\s*[<"]uv\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"uv_tcp_init\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r'#include\s*[<"]event2/', Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"evconnlistener_new_bind\b", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]grpcpp/', Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"grpc::Server\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"grpc::ServerBuilder\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"grpc::ClientContext\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]fstream[">]',
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"\bstd::ifstream\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"\bstd::ofstream\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bstd::fstream\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bfopen\s*\(", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
                (r"\bfread\s*\(", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"\bfwrite\s*\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (
                    r'#include\s*[<"]filesystem[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (r"\bstd::filesystem\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]hiredis/', Confidence.HIGH, SignalDirection.OUTWARD),
                (r"redisConnect\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"redisCommand\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r'#include\s*[<"]libmemcached/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"memcached_create\b", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r'#include\s*"soapH\.h"', Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bsoap_init\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bsoap_call_\w+", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]libssh2_sftp\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
                (r"libssh2_sftp_", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"curl_easy_setopt.*CURLPROTO_FTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\btimer_create\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"\bsetitimer\b", Confidence.HIGH, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]mailio/', Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bmailio::message\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bmailio::smtp\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bmailio::smtps\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bmailio::imap\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bmailio::imaps\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bmailio::pop3\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r'#include\s*[<"]vmime/', Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bvmime::net::smtp\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\bvmime::net::imap\b", Confidence.HIGH, SignalDirection.OUTWARD),
                (
                    r"\bvmime::messageBuilder\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [],
        },
    },
)
