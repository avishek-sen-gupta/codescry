"""C# base integration patterns."""

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
                (
                    r"\[ApiController\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "REST controller is annotated with ApiController",
                        "REST API controller is exposed for inbound requests",
                    ),
                ),
                (
                    r"\[HttpGet\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP GET endpoint is defined with attribute",
                        "HTTP GET request is handled for inbound processing",
                    ),
                ),
                (
                    r"\[HttpPost\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP POST endpoint is defined with attribute",
                        "HTTP POST request is handled for inbound processing",
                    ),
                ),
                (
                    r"\[HttpPut\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP PUT endpoint is defined with attribute",
                        "HTTP PUT request is handled for inbound processing",
                    ),
                ),
                (
                    r"\[HttpDelete\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "HTTP DELETE endpoint is defined with attribute",
                        "HTTP DELETE request is handled for inbound processing",
                    ),
                ),
                (
                    r"\[Route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "URL route is declared for controller action",
                        "HTTP endpoint is exposed with routing",
                    ),
                ),
                (
                    r"\[FromBody\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Request body is bound to action parameter",
                        "Request body data is received from inbound request",
                    ),
                ),
                (
                    r"\[FromQuery\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Query string is bound to action parameter",
                        "Query string data is received from inbound request",
                    ),
                ),
                (
                    r"ControllerBase",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "REST controller is extended from base",
                        "REST API controller is exposed for inbound requests",
                    ),
                ),
                (
                    r"IActionResult",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is represented via interface",
                        "HTTP request is handled and responded to",
                    ),
                ),
                (
                    r"ActionResult<",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "HTTP response is wrapped with type",
                        "HTTP request is handled and responded to",
                    ),
                ),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (
                    r"\[ServiceContract\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WCF service interface is declared with attribute",
                        "SOAP service contract is exposed for inbound requests",
                    ),
                ),
                (
                    r"\[OperationContract\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WCF service operation is declared with attribute",
                        "SOAP service operation is exposed for inbound requests",
                    ),
                ),
                (
                    r"\[DataContract\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "WCF serialization is marked with DataContract",
                        "Data type is defined for inbound SOAP messages",
                    ),
                ),
                (
                    r"\[DataMember\]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "WCF serialization is marked with DataMember",
                        "Member field is defined for inbound SOAP messages",
                    ),
                ),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (
                    r"using MassTransit",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Message bus is integrated via namespace",
                        "Message bus is accessed",
                    ),
                ),
                (
                    r"using NServiceBus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Service bus is integrated via namespace",
                        "Service bus is accessed",
                    ),
                ),
                (
                    r"using Rebus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Message bus is integrated via Rebus",
                        "Message bus is accessed",
                    ),
                ),
                (
                    r"IConsumer<",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Messages are consumed via interface",
                        "Message is received and consumed from bus",
                    ),
                ),
                (
                    r"IMessageHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Inbound messages are handled via interface",
                        "Incoming messages are handled via messaging",
                    ),
                ),
                (
                    r"IBus\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Messages are sent via bus interface",
                        "Outbound messages are sent to message bus",
                    ),
                ),
                (
                    r"Amazon\.SQS",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SQS queue is accessed with client",
                        "SQS message queue is accessed",
                    ),
                ),
                (
                    r"Amazon\.SimpleNotificationService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "SNS notifications are sent with client",
                        "SNS notification topic is accessed",
                    ),
                ),
                (
                    r"Azure\.Messaging\.ServiceBus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Service Bus is accessed with client",
                        "Azure Service Bus is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"using Microsoft\.AspNetCore\.SignalR",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "Real-time communication is enabled via SignalR",
                        "WebSocket connections are accepted for real-time communication",
                    ),
                ),
                (
                    r": Hub\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "SignalR hub is defined for clients",
                        "Real-time communication hub is exposed for inbound connections",
                    ),
                ),
                (
                    r"HubConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SignalR hub is connected remotely",
                        "Real-time hub is connected for outbound SignalR",
                    ),
                ),
                (
                    r"TcpClient",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "TCP connection is opened for outbound communication",
                        "Outbound TCP endpoint is connected via networking",
                    ),
                ),
                (
                    r"TcpListener",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "TCP connections are listened for inbound traffic",
                        "Inbound TCP connections are accepted via networking",
                    ),
                ),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (
                    r"using.*EntityFramework",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is accessed via Entity Framework",
                        "Database is accessed",
                    ),
                ),
                (
                    r": DbContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database session is managed with context",
                        "Database is accessed",
                    ),
                ),
                (
                    r"DbSet<",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is represented as property",
                        "Database table is queried",
                    ),
                ),
                (
                    r"\[Table\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database table is mapped with class attribute",
                        "Database table is written with mapping",
                    ),
                ),
                (
                    r"\[Key\]",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database primary key is marked with attribute",
                        "Database primary key is defined",
                    ),
                ),
                (
                    r"\[Column\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "Database column is mapped with property attribute",
                        "Database column is written with mapping",
                    ),
                ),
                (
                    r"SqlConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SQL database connection is opened",
                        "SQL Server database is connected with credentials",
                    ),
                ),
                (
                    r"using Dapper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Database is accessed via lightweight ORM",
                        "Database is queried with mapper",
                    ),
                ),
                (
                    r"Amazon\.DynamoDBv2",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "DynamoDB is accessed with client",
                        "DynamoDB database is written to",
                    ),
                ),
                (
                    r"Microsoft\.Azure\.Cosmos",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cosmos DB is accessed with client",
                        "Azure Cosmos DB database is queried",
                    ),
                ),
                (
                    r"using Neo4j\.Driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is accessed via driver",
                        "Neo4j graph database is connected",
                    ),
                ),
                (
                    r"IDriver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Graph database is accessed via driver",
                        "Neo4j graph database is connected",
                    ),
                ),
                (
                    r"GraphDatabase\.Driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Neo4j driver is created with factory",
                        "Neo4j graph database is connected",
                    ),
                ),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (
                    r"File\.Read",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Filesystem is read with method",
                        "File is read via System.IO",
                    ),
                ),
                (
                    r"File\.Write",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Filesystem is written with method",
                        "File is written via System.IO",
                    ),
                ),
                (
                    r"StreamReader",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Character data is read from stream",
                        "Stream is read via System.IO",
                    ),
                ),
                (
                    r"StreamWriter",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Character data is written to stream",
                        "Stream is written via System.IO",
                    ),
                ),
                (
                    r"AWSSDK\.S3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "S3 storage is accessed with client",
                        "S3 storage bucket is written to",
                    ),
                ),
                (
                    r"Azure\.Storage\.Blobs",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Blob storage is accessed with client",
                        "Azure Blob Storage is written to",
                    ),
                ),
                (
                    r"Google\.Cloud\.Storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Cloud Storage is accessed with client",
                        "Google Cloud Storage is written to",
                    ),
                ),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (
                    r"Grpc\.Core",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "gRPC primitives are provided via library",
                        "gRPC service is accessed via client",
                    ),
                ),
                (
                    r"Grpc\.Net\.Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "gRPC client is created with library",
                        "gRPC service is called for outbound requests",
                    ),
                ),
                (
                    r"ServerServiceDefinition",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "gRPC service is registered with definition",
                        "gRPC service is exposed for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (
                    r"HotChocolate",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL server is exposed with library",
                        "GraphQL API is exposed for inbound requests",
                    ),
                ),
                (
                    r"GraphQL\.Server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "GraphQL endpoint is hosted with library",
                        "GraphQL API is exposed for inbound requests",
                    ),
                ),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (
                    r"System\.Net\.Mail",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SMTP email is sent via namespace",
                        "Email messages are sent via outbound SMTP",
                    ),
                ),
                (
                    r"SmtpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent via SMTP",
                        "Email message is sent via SMTP",
                    ),
                ),
                (
                    r"MailKit",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email is sent and received",
                        "Email message is sent via SMTP",
                    ),
                ),
                (
                    r"MimeKit",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Email messages are constructed with MIME",
                        "MIME email message is produced for outbound delivery",
                    ),
                ),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (
                    r"IDistributedCache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Distributed cache is accessed via interface",
                        "Remote cache store is written via distributed caching",
                    ),
                ),
                (
                    r"IMemoryCache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Memory cache is accessed via interface",
                        "In-process cache is written via memory caching",
                    ),
                ),
                (
                    r"StackExchange\.Redis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "Redis cache is connected with client",
                        "Redis cache is written with key-value data",
                    ),
                ),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (
                    r"IAsyncEnumerable",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "Data is streamed asynchronously",
                        "Data stream is exposed for inbound async streaming",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Server-Sent Events are enabled with content type",
                        "SSE stream is accessed",
                    ),
                ),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (
                    r"FtpWebRequest",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP request is made outbound",
                        "Outbound FTP server is connected via networking",
                    ),
                ),
                (
                    r"SftpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "SFTP server is connected outbound",
                        "SFTP server is connected for outbound access",
                    ),
                ),
                (
                    r"FluentFTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "FTP server is connected via library",
                        "FTP server is connected for outbound access",
                    ),
                ),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (
                    r"Hangfire",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background jobs are scheduled with library",
                        "Background job scheduler is integrated",
                    ),
                ),
                (
                    r"Quartz",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Jobs are scheduled with Quartz",
                        "Job scheduling system is integrated",
                    ),
                ),
                (
                    r"IHostedService",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "Background service is hosted via interface",
                        "Background service lifecycle is integrated with hosting",
                    ),
                ),
            ],
        },
    },
)
