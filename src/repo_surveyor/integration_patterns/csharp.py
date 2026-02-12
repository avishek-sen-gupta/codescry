"""C# integration patterns."""

from .types import Confidence, IntegrationType

BASE_PATTERNS = {
    IntegrationType.HTTP_REST: {
        "patterns": [
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
        "patterns": [
            (r"\[ServiceContract\]", Confidence.HIGH),
            (r"\[OperationContract\]", Confidence.HIGH),
            (r"\[DataContract\]", Confidence.HIGH),
            (r"\[DataMember\]", Confidence.MEDIUM),
        ],
    },
    IntegrationType.MESSAGING: {
        "patterns": [
            (r"using MassTransit", Confidence.HIGH),
            (r"using NServiceBus", Confidence.HIGH),
            (r"using Rebus", Confidence.HIGH),
            (r"IConsumer<", Confidence.HIGH),
            (r"IMessageHandler", Confidence.HIGH),
            (r"IBus\b", Confidence.MEDIUM),
        ],
    },
    IntegrationType.SOCKET: {
        "patterns": [
            (r"using Microsoft\.AspNetCore\.SignalR", Confidence.HIGH),
            (r": Hub\b", Confidence.HIGH),
            (r"HubConnection", Confidence.HIGH),
            (r"TcpClient", Confidence.MEDIUM),
            (r"TcpListener", Confidence.MEDIUM),
        ],
    },
    IntegrationType.DATABASE: {
        "patterns": [
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
        "patterns": [
            (r"File\.Read", Confidence.HIGH),
            (r"File\.Write", Confidence.HIGH),
            (r"StreamReader", Confidence.HIGH),
            (r"StreamWriter", Confidence.HIGH),
            (r"AWSSDK\.S3", Confidence.HIGH),
            (r"FtpWebRequest", Confidence.HIGH),
        ],
    },
    IntegrationType.GRPC: {
        "patterns": [
            (r"Grpc\.Core", Confidence.HIGH),
            (r"Grpc\.Net\.Client", Confidence.HIGH),
            (r"ServerServiceDefinition", Confidence.HIGH),
        ],
    },
}

FRAMEWORK_PATTERNS = {
    "ASP.NET Core": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"IApplicationBuilder", Confidence.HIGH),
                (r"app\.MapGet", Confidence.HIGH),
                (r"app\.MapPost", Confidence.HIGH),
                (r"WebApplication\.CreateBuilder", Confidence.HIGH),
                (r"MinimalApi", Confidence.HIGH),
            ],
        },
        IntegrationType.GRPC: {
            "patterns": [
                (r"MapGrpcService", Confidence.HIGH),
                (r"Grpc\.AspNetCore", Confidence.HIGH),
            ],
        },
    },
    "WCF": {
        IntegrationType.SOAP: {
            "patterns": [
                (r"System\.ServiceModel", Confidence.HIGH),
                (r"ChannelFactory", Confidence.HIGH),
                (r"BasicHttpBinding", Confidence.HIGH),
                (r"ServiceHost", Confidence.HIGH),
            ],
        },
    },
    "CoreWCF": {
        IntegrationType.SOAP: {
            "patterns": [
                (r"using CoreWCF", Confidence.HIGH),
                (r"CoreWCF\.Http", Confidence.HIGH),
            ],
        },
    },
    "ServiceStack": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"using ServiceStack", Confidence.HIGH),
                (r"IReturn<", Confidence.HIGH),
                (r"IGet\b", Confidence.HIGH),
                (r"IPost\b", Confidence.HIGH),
            ],
        },
    },
    "Nancy": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"NancyModule", Confidence.HIGH),
                (r"Get\[", Confidence.HIGH),
                (r"Post\[", Confidence.HIGH),
            ],
        },
    },
    "Carter": {
        IntegrationType.HTTP_REST: {
            "patterns": [
                (r"CarterModule", Confidence.HIGH),
                (r"ICarterModule", Confidence.HIGH),
            ],
        },
    },
}
