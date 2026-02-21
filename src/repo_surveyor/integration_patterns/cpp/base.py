"""C/C++ base integration patterns."""

from repo_surveyor.integration_patterns.types import (
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
                    (
                        "C++ sqlite3.h header import for SQLite database access",
                        "This code uses C++ SQLite3 to connect to a local SQLite database",
                    ),
                ),
                (
                    r"sqlite3_open\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ sqlite3_open call opening a SQLite database connection",
                        "This code uses C++ SQLite3 to connect to a SQLite database",
                    ),
                ),
                (
                    r"sqlite3_exec\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ sqlite3_exec call executing a SQL statement",
                        "This code uses C++ SQLite3 to query a SQLite database",
                    ),
                ),
                (
                    r"sqlite3_prepare\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ sqlite3_prepare call preparing a SQL statement",
                        "This code uses C++ SQLite3 to execute outbound database queries",
                    ),
                ),
                (
                    r'#include\s*[<"]libpq-fe\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ libpq-fe.h header import for PostgreSQL client access",
                        "This code uses C++ libpq to connect to a PostgreSQL database",
                    ),
                ),
                (
                    r"PQconnectdb\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ PQconnectdb call opening a PostgreSQL database connection",
                        "This code uses C++ libpq to connect to a PostgreSQL database",
                    ),
                ),
                (
                    r"PQexec\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ PQexec call executing a PostgreSQL query",
                        "This code uses C++ libpq to query a PostgreSQL database",
                    ),
                ),
                (
                    r'#include\s*[<"]mysql\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mysql.h header import for MySQL client access",
                        "This code uses C++ MySQL C API to connect to a MySQL database",
                    ),
                ),
                (
                    r"mysql_real_connect\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mysql_real_connect call opening a MySQL database connection",
                        "This code uses C++ MySQL C API to connect to a MySQL database",
                    ),
                ),
                (
                    r"mysql_query\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mysql_query call executing a MySQL query",
                        "This code uses C++ MySQL C API to query a MySQL database",
                    ),
                ),
                (
                    r'#include\s*[<"]mongoc\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mongoc.h header import for MongoDB C driver access",
                        "This code uses C++ MongoDB C driver to connect to a MongoDB database",
                    ),
                ),
                (
                    r'#include\s*[<"]sql\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ sql.h ODBC header import for database access",
                        "This code uses C++ ODBC to connect to a relational database",
                    ),
                ),
                (
                    r'#include\s*[<"]sqlext\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ sqlext.h ODBC extension header import for database access",
                        "This code uses C++ ODBC extensions to query a relational database",
                    ),
                ),
                (
                    r'#include\s*[<"]leveldb/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ LevelDB header import for key-value store access",
                        "This code uses C++ LevelDB to write to a key-value store",
                    ),
                ),
                (
                    r'#include\s*[<"]rocksdb/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ RocksDB header import for key-value store access",
                        "This code uses C++ RocksDB to write to a key-value store",
                    ),
                ),
                (
                    r'#include\s*[<"]lmdb\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ lmdb.h header import for LMDB embedded database access",
                        "This code uses C++ LMDB to write to an embedded key-value store",
                    ),
                ),
            ],
        },
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]curl/curl\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ curl/curl.h header import for libcurl HTTP client",
                        "This code uses C++ libcurl to send outbound HTTP requests",
                    ),
                ),
                (
                    r"curl_easy_init\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ curl_easy_init call initializing a libcurl easy handle",
                        "This code uses C++ libcurl to send outbound HTTP requests",
                    ),
                ),
                (
                    r"curl_easy_perform\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ curl_easy_perform call executing an HTTP transfer",
                        "This code uses C++ libcurl to send outbound HTTP requests",
                    ),
                ),
                (
                    r"curl_easy_setopt\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ curl_easy_setopt call configuring a libcurl transfer",
                        "This code uses C++ libcurl to configure outbound HTTP requests",
                    ),
                ),
                (
                    r'#include\s*[<"]cpprest/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ cpprestsdk header import for REST client/server",
                        "This code uses C++ cpprestsdk to interact with an HTTP endpoint",
                    ),
                ),
                (
                    r'#include\s*[<"]microhttpd\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C++ microhttpd.h header import for embedded HTTP server",
                        "This code uses C++ libmicrohttpd to expose an inbound HTTP server",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]librdkafka/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ librdkafka header import for Kafka client",
                        "This code uses C++ librdkafka to interact with a Kafka broker",
                    ),
                ),
                (
                    r"rd_kafka_new\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ rd_kafka_new call creating a Kafka client instance",
                        "This code uses C++ librdkafka to interact with a Kafka broker",
                    ),
                ),
                (
                    r'#include\s*[<"]amqp\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ amqp.h header import for AMQP messaging client",
                        "This code uses C++ rabbitmq-c to interact with an AMQP broker",
                    ),
                ),
                (
                    r"amqp_new_connection\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ amqp_new_connection call establishing an AMQP connection",
                        "This code uses C++ rabbitmq-c to interact with an AMQP broker",
                    ),
                ),
                (
                    r'#include\s*[<"]zmq\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ zmq.h header import for ZeroMQ messaging",
                        "This code uses C++ ZeroMQ to interact with a messaging socket",
                    ),
                ),
                (
                    r"zmq_socket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ zmq_socket call creating a ZeroMQ socket",
                        "This code uses C++ ZeroMQ to interact with a messaging socket",
                    ),
                ),
                (
                    r'#include\s*[<"]nng/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ nng header import for nanomsg next generation messaging",
                        "This code uses C++ nng to interact with a messaging socket",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]sys/socket\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ sys/socket.h header import for POSIX socket API",
                        "This code uses C++ POSIX sockets to interact with a network socket",
                    ),
                ),
                (
                    r'#include\s*[<"]netinet/in\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ netinet/in.h header import for internet address structures",
                        "This code uses C++ POSIX sockets to interact with a network socket",
                    ),
                ),
                (
                    r'#include\s*[<"]uv\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ uv.h header import for libuv async I/O",
                        "This code uses C++ libuv to interact with asynchronous network I/O",
                    ),
                ),
                (
                    r"uv_tcp_init\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ uv_tcp_init call initializing a libuv TCP handle",
                        "This code uses C++ libuv to interact with a TCP socket",
                    ),
                ),
                (
                    r'#include\s*[<"]event2/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ libevent2 header import for event-driven networking",
                        "This code uses C++ libevent to interact with event-driven network I/O",
                    ),
                ),
                (
                    r"evconnlistener_new_bind\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C++ evconnlistener_new_bind call binding a libevent connection listener",
                        "This code uses C++ libevent to accept inbound network connections",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]grpcpp/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ grpcpp header import for gRPC C++ library",
                        "This code uses C++ gRPC to interact with a gRPC endpoint",
                    ),
                ),
                (
                    r"grpc::Server\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C++ grpc::Server class instantiation for gRPC server",
                        "This code uses C++ gRPC to expose an inbound gRPC server",
                    ),
                ),
                (
                    r"grpc::ServerBuilder\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C++ grpc::ServerBuilder class building a gRPC server",
                        "This code uses C++ gRPC to expose an inbound gRPC server",
                    ),
                ),
                (
                    r"grpc::ClientContext\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ grpc::ClientContext class for outbound gRPC calls",
                        "This code uses C++ gRPC to call an outbound gRPC service",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]fstream[">]',
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ fstream header import for file stream I/O",
                        "This code uses C++ fstream to interact with file I/O",
                    ),
                ),
                (
                    r"\bstd::ifstream\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C++ std::ifstream class reading from a file",
                        "This code uses C++ std::ifstream to receive data from a file",
                    ),
                ),
                (
                    r"\bstd::ofstream\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ std::ofstream class writing to a file",
                        "This code uses C++ std::ofstream to write to a file",
                    ),
                ),
                (
                    r"\bstd::fstream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ std::fstream class for bidirectional file I/O",
                        "This code uses C++ std::fstream to interact with a file",
                    ),
                ),
                (
                    r"\bfopen\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ fopen call opening a file for reading or writing",
                        "This code uses C++ fopen to interact with a file",
                    ),
                ),
                (
                    r"\bfread\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "C++ fread call reading data from a file stream",
                        "This code uses C++ fread to receive data from a file",
                    ),
                ),
                (
                    r"\bfwrite\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "C++ fwrite call writing data to a file stream",
                        "This code uses C++ fwrite to write to a file",
                    ),
                ),
                (
                    r'#include\s*[<"]filesystem[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ filesystem header import for std::filesystem operations",
                        "This code uses C++ std::filesystem to interact with the file system",
                    ),
                ),
                (
                    r"\bstd::filesystem\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ std::filesystem namespace usage for file system operations",
                        "This code uses C++ std::filesystem to interact with the file system",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]hiredis/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ hiredis header import for Redis client",
                        "This code uses C++ hiredis to connect to a Redis cache",
                    ),
                ),
                (
                    r"redisConnect\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ redisConnect call connecting to a Redis server",
                        "This code uses C++ hiredis to connect to a Redis cache",
                    ),
                ),
                (
                    r"redisCommand\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ redisCommand call sending a command to Redis",
                        "This code uses C++ hiredis to query a Redis cache",
                    ),
                ),
                (
                    r'#include\s*[<"]libmemcached/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ libmemcached header import for Memcached client",
                        "This code uses C++ libmemcached to connect to a Memcached cache",
                    ),
                ),
                (
                    r"memcached_create\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ memcached_create call creating a Memcached client instance",
                        "This code uses C++ libmemcached to connect to a Memcached cache",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*"soapH\.h"',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ soapH.h gSOAP header import for SOAP web services",
                        "This code uses C++ gSOAP to interact with a SOAP web service",
                    ),
                ),
                (
                    r"\bsoap_init\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ soap_init call initializing a gSOAP context",
                        "This code uses C++ gSOAP to interact with a SOAP web service",
                    ),
                ),
                (
                    r"\bsoap_call_\w+",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ soap_call_* stub invoking an outbound SOAP operation",
                        "This code uses C++ gSOAP to call an outbound SOAP web service",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]libssh2_sftp\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ libssh2_sftp.h header import for SFTP client",
                        "This code uses C++ libssh2 to connect to an SFTP server",
                    ),
                ),
                (
                    r"libssh2_sftp_",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ libssh2_sftp_* call performing an SFTP operation",
                        "This code uses C++ libssh2 to connect to an SFTP server",
                    ),
                ),
                (
                    r"curl_easy_setopt.*CURLPROTO_FTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ curl_easy_setopt with CURLPROTO_FTP configuring an FTP transfer",
                        "This code uses C++ libcurl to connect to an FTP server",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"\btimer_create\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ timer_create call creating a POSIX interval timer",
                        "This code uses C++ POSIX timers to interact with scheduled tasks",
                    ),
                ),
                (
                    r"\bsetitimer\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C++ setitimer call setting an interval timer",
                        "This code uses C++ setitimer to interact with scheduled tasks",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r'#include\s*[<"]mailio/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mailio header import for email library",
                        "This code uses C++ mailio to send outbound email messages",
                    ),
                ),
                (
                    r"\bmailio::message\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mailio::message class composing an email message",
                        "This code uses C++ mailio to send outbound email messages",
                    ),
                ),
                (
                    r"\bmailio::smtp\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mailio::smtp class sending email via SMTP",
                        "This code uses C++ mailio to send outbound email via SMTP",
                    ),
                ),
                (
                    r"\bmailio::smtps\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mailio::smtps class sending email via SMTPS",
                        "This code uses C++ mailio to send outbound email via SMTPS",
                    ),
                ),
                (
                    r"\bmailio::imap\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mailio::imap class accessing email via IMAP",
                        "This code uses C++ mailio to connect to an IMAP mail server",
                    ),
                ),
                (
                    r"\bmailio::imaps\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mailio::imaps class accessing email via IMAPS",
                        "This code uses C++ mailio to connect to an IMAPS mail server",
                    ),
                ),
                (
                    r"\bmailio::pop3\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ mailio::pop3 class retrieving email via POP3",
                        "This code uses C++ mailio to connect to a POP3 mail server",
                    ),
                ),
                (
                    r'#include\s*[<"]vmime/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ vmime header import for email library",
                        "This code uses C++ VMime to send outbound email messages",
                    ),
                ),
                (
                    r"\bvmime::net::smtp\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ vmime::net::smtp namespace sending email via SMTP",
                        "This code uses C++ VMime to send outbound email via SMTP",
                    ),
                ),
                (
                    r"\bvmime::net::imap\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ vmime::net::imap namespace accessing email via IMAP",
                        "This code uses C++ VMime to connect to an IMAP mail server",
                    ),
                ),
                (
                    r"\bvmime::messageBuilder\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C++ vmime::messageBuilder class composing an email message",
                        "This code uses C++ VMime to send outbound email messages",
                    ),
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
