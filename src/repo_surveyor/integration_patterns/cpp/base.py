"""C/C++ base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]sqlite3\.h[">]', Confidence.HIGH),
                (r"sqlite3_open\b", Confidence.HIGH),
                (r"sqlite3_exec\b", Confidence.HIGH),
                (r"sqlite3_prepare\b", Confidence.HIGH),
                (r'#include\s*[<"]libpq-fe\.h[">]', Confidence.HIGH),
                (r"PQconnectdb\b", Confidence.HIGH),
                (r"PQexec\b", Confidence.HIGH),
                (r'#include\s*[<"]mysql\.h[">]', Confidence.HIGH),
                (r"mysql_real_connect\b", Confidence.HIGH),
                (r"mysql_query\b", Confidence.HIGH),
                (r'#include\s*[<"]mongoc\.h[">]', Confidence.HIGH),
                (r'#include\s*[<"]sql\.h[">]', Confidence.HIGH),
                (r'#include\s*[<"]sqlext\.h[">]', Confidence.HIGH),
                (r'#include\s*[<"]leveldb/', Confidence.HIGH),
                (r'#include\s*[<"]rocksdb/', Confidence.HIGH),
                (r'#include\s*[<"]lmdb\.h[">]', Confidence.HIGH),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]curl/curl\.h[">]', Confidence.HIGH),
                (r"curl_easy_init\b", Confidence.HIGH),
                (r"curl_easy_perform\b", Confidence.HIGH),
                (r"curl_easy_setopt\b", Confidence.HIGH),
                (r'#include\s*[<"]cpprest/', Confidence.HIGH),
                (r'#include\s*[<"]microhttpd\.h[">]', Confidence.HIGH),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]librdkafka/', Confidence.HIGH),
                (r"rd_kafka_new\b", Confidence.HIGH),
                (r'#include\s*[<"]amqp\.h[">]', Confidence.HIGH),
                (r"amqp_new_connection\b", Confidence.HIGH),
                (r'#include\s*[<"]zmq\.h[">]', Confidence.HIGH),
                (r"zmq_socket\b", Confidence.HIGH),
                (r'#include\s*[<"]nng/', Confidence.HIGH),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]sys/socket\.h[">]', Confidence.HIGH),
                (r'#include\s*[<"]netinet/in\.h[">]', Confidence.HIGH),
                (r'#include\s*[<"]uv\.h[">]', Confidence.HIGH),
                (r"uv_tcp_init\b", Confidence.HIGH),
                (r'#include\s*[<"]event2/', Confidence.HIGH),
                (r"evconnlistener_new_bind\b", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]grpcpp/', Confidence.HIGH),
                (r"grpc::Server\b", Confidence.HIGH),
                (r"grpc::ServerBuilder\b", Confidence.HIGH),
                (r"grpc::ClientContext\b", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]fstream[">]', Confidence.MEDIUM),
                (r"\bstd::ifstream\b", Confidence.HIGH),
                (r"\bstd::ofstream\b", Confidence.HIGH),
                (r"\bstd::fstream\b", Confidence.HIGH),
                (r"\bfopen\s*\(", Confidence.MEDIUM),
                (r"\bfread\s*\(", Confidence.MEDIUM),
                (r"\bfwrite\s*\(", Confidence.MEDIUM),
                (r'#include\s*[<"]filesystem[">]', Confidence.HIGH),
                (r"\bstd::filesystem\b", Confidence.HIGH),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]hiredis/', Confidence.HIGH),
                (r"redisConnect\b", Confidence.HIGH),
                (r"redisCommand\b", Confidence.HIGH),
                (r'#include\s*[<"]libmemcached/', Confidence.HIGH),
                (r"memcached_create\b", Confidence.HIGH),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r'#include\s*"soapH\.h"', Confidence.HIGH),
                (r"\bsoap_init\b", Confidence.HIGH),
                (r"\bsoap_call_\w+", Confidence.HIGH),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"\btimer_create\b", Confidence.HIGH),
                (r"\bsetitimer\b", Confidence.HIGH),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r'#include\s*[<"]mailio/', Confidence.HIGH),
                (r"\bmailio::message\b", Confidence.HIGH),
                (r"\bmailio::smtp\b", Confidence.HIGH),
                (r"\bmailio::smtps\b", Confidence.HIGH),
                (r"\bmailio::imap\b", Confidence.HIGH),
                (r"\bmailio::imaps\b", Confidence.HIGH),
                (r"\bmailio::pop3\b", Confidence.HIGH),
                (r'#include\s*[<"]vmime/', Confidence.HIGH),
                (r"\bvmime::net::smtp\b", Confidence.HIGH),
                (r"\bvmime::net::imap\b", Confidence.HIGH),
                (r"\bvmime::messageBuilder\b", Confidence.HIGH),
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
