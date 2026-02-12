"""C# base integration patterns."""

from ..types import BasePatternSpec, Confidence, IntegrationType, PatternKey

BASE = BasePatternSpec(
    patterns={
        IntegrationType.HTTP_REST: {
            PatternKey.PATTERNS: [
                (r"\[ApiController\]", Confidence.HIGH),
                (r"\[HttpGet\]", Confidence.HIGH),
                (r"\[HttpPost\]", Confidence.HIGH),
                (r"\[HttpPut\]", Confidence.HIGH),
                (r"\[HttpDelete\]", Confidence.HIGH),
                (r"\[Route\(", Confidence.HIGH),
                (r"\[FromBody\]", Confidence.HIGH),
                (r"\[FromQuery\]", Confidence.HIGH),
                (r"ControllerBase", Confidence.HIGH),
                (r"IActionResult", Confidence.MEDIUM),
                (r"ActionResult<", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOAP: {
            PatternKey.PATTERNS: [
                (r"\[ServiceContract\]", Confidence.HIGH),
                (r"\[OperationContract\]", Confidence.HIGH),
                (r"\[DataContract\]", Confidence.HIGH),
                (r"\[DataMember\]", Confidence.MEDIUM),
            ],
        },
        IntegrationType.MESSAGING: {
            PatternKey.PATTERNS: [
                (r"using MassTransit", Confidence.HIGH),
                (r"using NServiceBus", Confidence.HIGH),
                (r"using Rebus", Confidence.HIGH),
                (r"IConsumer<", Confidence.HIGH),
                (r"IMessageHandler", Confidence.HIGH),
                (r"IBus\b", Confidence.MEDIUM),
            ],
        },
        IntegrationType.SOCKET: {
            PatternKey.PATTERNS: [
                (r"using Microsoft\.AspNetCore\.SignalR", Confidence.HIGH),
                (r": Hub\b", Confidence.HIGH),
                (r"HubConnection", Confidence.HIGH),
                (r"TcpClient", Confidence.MEDIUM),
                (r"TcpListener", Confidence.MEDIUM),
            ],
        },
        IntegrationType.DATABASE: {
            PatternKey.PATTERNS: [
                (r"using.*EntityFramework", Confidence.HIGH),
                (r": DbContext", Confidence.HIGH),
                (r"DbSet<", Confidence.HIGH),
                (r"\[Table\(", Confidence.HIGH),
                (r"\[Key\]", Confidence.MEDIUM),
                (r"\[Column\(", Confidence.MEDIUM),
                (r"SqlConnection", Confidence.HIGH),
                (r"using Dapper", Confidence.HIGH),
            ],
        },
        IntegrationType.FILE_IO: {
            PatternKey.PATTERNS: [
                (r"File\.Read", Confidence.HIGH),
                (r"File\.Write", Confidence.HIGH),
                (r"StreamReader", Confidence.HIGH),
                (r"StreamWriter", Confidence.HIGH),
                (r"AWSSDK\.S3", Confidence.HIGH),
                (r"FtpWebRequest", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            PatternKey.PATTERNS: [
                (r"Grpc\.Core", Confidence.HIGH),
                (r"Grpc\.Net\.Client", Confidence.HIGH),
                (r"ServerServiceDefinition", Confidence.HIGH),
            ],
        },
    },
)
