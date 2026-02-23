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
                        "SQLite header is imported for database access",
                        "Local SQLite database is connected via SQLite3",
                    ),
                ),
                (
                    r"sqlite3_open\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQLite connection is opened to database",
                        "SQLite database is connected via SQLite3",
                    ),
                ),
                (
                    r"sqlite3_exec\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL statement is executed on database",
                        "SQLite database is queried via SQLite3",
                    ),
                ),
                (
                    r"sqlite3_prepare\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL statement is prepared for execution",
                        "Database queries are executed via outbound SQLite3",
                    ),
                ),
                (
                    r'#include\s*[<"]libpq-fe\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL client is imported via header",
                        "PostgreSQL connection is opened with database",
                    ),
                ),
                (
                    r"PQconnectdb\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL connection is opened with credentials",
                        "PostgreSQL connection is opened with database",
                    ),
                ),
                (
                    r"PQexec\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "PostgreSQL query is executed with statement",
                        "PostgreSQL database is queried with SQL",
                    ),
                ),
                (
                    r'#include\s*[<"]mysql\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL header is imported for client access",
                        "MySQL database is connected via C API",
                    ),
                ),
                (
                    r"mysql_real_connect\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL connection is opened to database",
                        "MySQL database is connected via C API",
                    ),
                ),
                (
                    r"mysql_query\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MySQL query is executed with statement",
                        "MySQL database is queried via C API",
                    ),
                ),
                (
                    r'#include\s*[<"]mongoc\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "MongoDB header is imported for C driver access",
                        "MongoDB database is connected via C driver",
                    ),
                ),
                (
                    r'#include\s*[<"]sql\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ODBC header is imported for database access",
                        "Relational database is connected via ODBC",
                    ),
                ),
                (
                    r'#include\s*[<"]sqlext\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "ODBC extension header is imported for database",
                        "Relational database is queried via ODBC extensions",
                    ),
                ),
                (
                    r'#include\s*[<"]leveldb/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Key-value store is imported via LevelDB",
                        "Key-value store is written via LevelDB",
                    ),
                ),
                (
                    r'#include\s*[<"]rocksdb/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Key-value store is imported via RocksDB",
                        "Key-value store is written via RocksDB",
                    ),
                ),
                (
                    r'#include\s*[<"]lmdb\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "LMDB database is imported for access",
                        "Embedded key-value store is written via LMDB",
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
                        "HTTP client is imported via libcurl",
                        "HTTP requests are sent via outbound libcurl",
                    ),
                ),
                (
                    r"curl_easy_init\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP handle is initialized for transfer",
                        "HTTP requests are sent via outbound libcurl",
                    ),
                ),
                (
                    r"curl_easy_perform\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP transfer is executed with request",
                        "HTTP requests are sent via outbound libcurl",
                    ),
                ),
                (
                    r"curl_easy_setopt\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "HTTP transfer is configured with options",
                        "HTTP requests are configured for outbound libcurl",
                    ),
                ),
                (
                    r'#include\s*[<"]cpprest/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "REST client is imported via cpprestsdk",
                        "HTTP endpoint is accessed via cpprestsdk",
                    ),
                ),
                (
                    r'#include\s*[<"]microhttpd\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP server is imported via microhttpd",
                        "HTTP server is exposed for inbound requests",
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
                        "Kafka client is imported via library",
                        "Kafka broker is connected for messaging",
                    ),
                ),
                (
                    r"rd_kafka_new\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Kafka client is created for messaging",
                        "Kafka broker is connected for messaging",
                    ),
                ),
                (
                    r'#include\s*[<"]amqp\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP messaging is imported via header",
                        "AMQP broker is connected for messaging",
                    ),
                ),
                (
                    r"amqp_new_connection\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "AMQP connection is established with broker",
                        "AMQP broker is connected for messaging",
                    ),
                ),
                (
                    r'#include\s*[<"]zmq\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "ZeroMQ header is imported for messaging",
                        "Messaging socket is accessed via ZeroMQ",
                    ),
                ),
                (
                    r"zmq_socket\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "ZeroMQ socket is created for messaging",
                        "Messaging socket is accessed via ZeroMQ",
                    ),
                ),
                (
                    r'#include\s*[<"]nng/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Nanomsg header is imported for messaging",
                        "Messaging socket is created for communication",
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
                        "Socket header is imported for POSIX API",
                        "Network socket is accessed via POSIX sockets",
                    ),
                ),
                (
                    r'#include\s*[<"]netinet/in\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Internet address header is imported for networking",
                        "Network socket is accessed via POSIX sockets",
                    ),
                ),
                (
                    r'#include\s*[<"]uv\.h[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Async I/O header is imported for libuv",
                        "Network I/O is handled asynchronously",
                    ),
                ),
                (
                    r"uv_tcp_init\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "TCP handle is initialized for networking",
                        "TCP socket is created for network communication",
                    ),
                ),
                (
                    r'#include\s*[<"]event2/',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Event networking is imported via libevent",
                        "Event-driven network I/O is handled via libevent",
                    ),
                ),
                (
                    r"evconnlistener_new_bind\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Connection listener is bound with libevent",
                        "Network connections are accepted via inbound libevent",
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
                        "gRPC library is imported for C++",
                        "gRPC endpoint is accessed via client",
                    ),
                ),
                (
                    r"grpc::Server\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC server is instantiated for service",
                        "gRPC server is exposed for inbound requests",
                    ),
                ),
                (
                    r"grpc::ServerBuilder\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC server is built with configuration",
                        "gRPC server is exposed for inbound requests",
                    ),
                ),
                (
                    r"grpc::ClientContext\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "gRPC calls are made with ClientContext",
                        "gRPC service is called for outbound requests",
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
                        "File stream is imported for I/O",
                        "File I/O is accessed via fstream",
                    ),
                ),
                (
                    r"\bstd::ifstream\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "File input stream is created for reading",
                        "File data is received from input stream",
                    ),
                ),
                (
                    r"\bstd::ofstream\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "File output stream is created for writing",
                        "File is written with output stream",
                    ),
                ),
                (
                    r"\bstd::fstream\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File stream is created for bidirectional I/O",
                        "File is opened for I/O operations",
                    ),
                ),
                (
                    r"\bfopen\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "File is opened for reading writing",
                        "File is accessed via fopen",
                    ),
                ),
                (
                    r"\bfread\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Data is read from file stream",
                        "Data is received from file via fread",
                    ),
                ),
                (
                    r"\bfwrite\s*\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Data is written to file stream",
                        "File is written via fwrite",
                    ),
                ),
                (
                    r'#include\s*[<"]filesystem[">]',
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem operations are imported via header",
                        "File system is accessed for operations",
                    ),
                ),
                (
                    r"\bstd::filesystem\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Filesystem operations are accessed via namespace",
                        "File system is accessed for operations",
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
                        "Redis client is imported via hiredis",
                        "Redis cache is connected via hiredis",
                    ),
                ),
                (
                    r"redisConnect\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis connection is opened to server",
                        "Redis cache is connected via hiredis",
                    ),
                ),
                (
                    r"redisCommand\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis command is sent to server",
                        "Redis cache is queried via hiredis",
                    ),
                ),
                (
                    r'#include\s*[<"]libmemcached/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached client is imported via library",
                        "Cache connection is opened with Memcached server",
                    ),
                ),
                (
                    r"memcached_create\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memcached client is created with instance",
                        "Cache connection is opened with Memcached server",
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
                        "SOAP header is imported for web services",
                        "SOAP web service is accessed via gSOAP",
                    ),
                ),
                (
                    r"\bsoap_init\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SOAP context is initialized for web services",
                        "SOAP web service is accessed via gSOAP",
                    ),
                ),
                (
                    r"\bsoap_call_\w+",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SOAP operation is invoked for outbound call",
                        "SOAP web service is called for outbound requests",
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
                        "SFTP client is imported via header",
                        "SFTP connection is opened with server",
                    ),
                ),
                (
                    r"libssh2_sftp_",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP operation is performed with call",
                        "SFTP connection is opened with server",
                    ),
                ),
                (
                    r"curl_easy_setopt.*CURLPROTO_FTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP transfer is configured with protocol",
                        "FTP server is connected via libcurl",
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
                        "POSIX timer is created with interval",
                        "Scheduled tasks are managed via POSIX timers",
                    ),
                ),
                (
                    r"\bsetitimer\b",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Interval timer is set with parameters",
                        "Scheduled tasks are created with timer",
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
                        "Email library is imported via mailio",
                        "Email message is sent outbound",
                    ),
                ),
                (
                    r"\bmailio::message\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is composed with class",
                        "Email message is sent outbound",
                    ),
                ),
                (
                    r"\bmailio::smtp\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent via SMTP client",
                        "Email is sent outbound via SMTP",
                    ),
                ),
                (
                    r"\bmailio::smtps\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent via SMTPS client",
                        "Email is sent outbound via SMTPS",
                    ),
                ),
                (
                    r"\bmailio::imap\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is accessed via IMAP client",
                        "IMAP connection is opened with mail server",
                    ),
                ),
                (
                    r"\bmailio::imaps\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is accessed via IMAPS client",
                        "IMAPS connection is opened with mail server",
                    ),
                ),
                (
                    r"\bmailio::pop3\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is retrieved via POP3 client",
                        "POP3 connection is opened with mail server",
                    ),
                ),
                (
                    r'#include\s*[<"]vmime/',
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email header is imported for messaging",
                        "Email messages are sent via outbound VMime",
                    ),
                ),
                (
                    r"\bvmime::net::smtp\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent via SMTP protocol",
                        "Email is sent via outbound SMTP VMime",
                    ),
                ),
                (
                    r"\bvmime::net::imap\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is accessed via IMAP protocol",
                        "IMAP mail server is connected via VMime",
                    ),
                ),
                (
                    r"\bvmime::messageBuilder\b",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email message is composed with builder",
                        "Email messages are sent via outbound VMime",
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
