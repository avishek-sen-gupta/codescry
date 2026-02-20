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
                (r"\[ApiController\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[HttpGet\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[HttpPost\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[HttpPut\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[HttpDelete\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[Route\(", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[FromBody\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[FromQuery\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"ControllerBase", Confidence.HIGH, SignalDirection.INWARD),
                (r"IActionResult", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"ActionResult<", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"\[ServiceContract\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[OperationContract\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[DataContract\]", Confidence.HIGH, SignalDirection.INWARD),
                (r"\[DataMember\]", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"using MassTransit", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"using NServiceBus", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"using Rebus", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"IConsumer<", Confidence.HIGH, SignalDirection.INWARD),
                (r"IMessageHandler", Confidence.HIGH, SignalDirection.INWARD),
                (r"IBus\b", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"Amazon\.SQS", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (
                    r"Amazon\.SimpleNotificationService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
                (
                    r"Azure\.Messaging\.ServiceBus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                ),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (
                    r"using Microsoft\.AspNetCore\.SignalR",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                ),
                (r": Hub\b", Confidence.HIGH, SignalDirection.INWARD),
                (r"HubConnection", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"TcpClient", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"TcpListener", Confidence.MEDIUM, SignalDirection.INWARD),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"using.*EntityFramework", Confidence.HIGH, SignalDirection.OUTWARD),
                (r": DbContext", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"DbSet<", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\[Table\(", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"\[Key\]", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"\[Column\(", Confidence.MEDIUM, SignalDirection.OUTWARD),
                (r"SqlConnection", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"using Dapper", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Amazon\.DynamoDBv2", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Microsoft\.Azure\.Cosmos", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"using Neo4j\.Driver", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"IDriver", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"GraphDatabase\.Driver", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"File\.Read", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"File\.Write", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"StreamReader", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"StreamWriter", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"AWSSDK\.S3", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Azure\.Storage\.Blobs", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"Google\.Cloud\.Storage", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"Grpc\.Core", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Grpc\.Net\.Client", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"ServerServiceDefinition", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.GRAPHQL: {
            PatternKey.PATTERNS: [
                (r"HotChocolate", Confidence.HIGH, SignalDirection.INWARD),
                (r"GraphQL\.Server", Confidence.HIGH, SignalDirection.INWARD),
            ],
        },
        IntegrationType.EMAIL: {
            PatternKey.PATTERNS: [
                (r"System\.Net\.Mail", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SmtpClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MailKit", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"MimeKit", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.CACHING: {
            PatternKey.PATTERNS: [
                (r"IDistributedCache", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"IMemoryCache", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"StackExchange\.Redis", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SSE_STREAMING: {
            PatternKey.PATTERNS: [
                (r"IAsyncEnumerable", Confidence.MEDIUM, SignalDirection.INWARD),
                (r"text/event-stream", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
        IntegrationType.FTP_SFTP: {
            PatternKey.PATTERNS: [
                (r"FtpWebRequest", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"SftpClient", Confidence.HIGH, SignalDirection.OUTWARD),
                (r"FluentFTP", Confidence.HIGH, SignalDirection.OUTWARD),
            ],
        },
        IntegrationType.SCHEDULING: {
            PatternKey.PATTERNS: [
                (r"Hangfire", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"Quartz", Confidence.HIGH, SignalDirection.AMBIGUOUS),
                (r"IHostedService", Confidence.MEDIUM, SignalDirection.AMBIGUOUS),
            ],
        },
    },
)
