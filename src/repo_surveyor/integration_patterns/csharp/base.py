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
                        "C# [ApiController] attribute marking a REST API controller",
                        "This code uses C# ASP.NET to expose an inbound REST API controller",
                    ),
                ),
                (
                    r"\[HttpGet\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [HttpGet] attribute defining an HTTP GET endpoint",
                        "This code uses C# ASP.NET to handle incoming HTTP GET requests",
                    ),
                ),
                (
                    r"\[HttpPost\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [HttpPost] attribute defining an HTTP POST endpoint",
                        "This code uses C# ASP.NET to handle incoming HTTP POST requests",
                    ),
                ),
                (
                    r"\[HttpPut\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [HttpPut] attribute defining an HTTP PUT endpoint",
                        "This code uses C# ASP.NET to handle incoming HTTP PUT requests",
                    ),
                ),
                (
                    r"\[HttpDelete\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [HttpDelete] attribute defining an HTTP DELETE endpoint",
                        "This code uses C# ASP.NET to handle incoming HTTP DELETE requests",
                    ),
                ),
                (
                    r"\[Route\(",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [Route] attribute declaring a URL route for a controller or action",
                        "This code uses C# ASP.NET to expose a routed HTTP endpoint",
                    ),
                ),
                (
                    r"\[FromBody\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [FromBody] attribute binding an action parameter from the request body",
                        "This code uses C# ASP.NET to receive data from an inbound HTTP request body",
                    ),
                ),
                (
                    r"\[FromQuery\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [FromQuery] attribute binding an action parameter from the query string",
                        "This code uses C# ASP.NET to receive data from an inbound HTTP query string",
                    ),
                ),
                (
                    r"ControllerBase",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# ControllerBase class serving as the base for REST API controllers",
                        "This code uses C# ASP.NET to expose an inbound REST API controller",
                    ),
                ),
                (
                    r"IActionResult",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "C# IActionResult interface representing an HTTP action response",
                        "This code uses C# ASP.NET to handle and respond to inbound HTTP requests",
                    ),
                ),
                (
                    r"ActionResult<",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "C# ActionResult<T> generic type wrapping a typed HTTP action response",
                        "This code uses C# ASP.NET to handle and respond to inbound HTTP requests",
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
                        "C# [ServiceContract] attribute declaring a WCF service interface",
                        "This code uses C# WCF to expose an inbound SOAP service contract",
                    ),
                ),
                (
                    r"\[OperationContract\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [OperationContract] attribute declaring a WCF service operation",
                        "This code uses C# WCF to expose an inbound SOAP service operation",
                    ),
                ),
                (
                    r"\[DataContract\]",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# [DataContract] attribute marking a type for WCF serialization",
                        "This code uses C# WCF to define a data type for inbound SOAP messages",
                    ),
                ),
                (
                    r"\[DataMember\]",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "C# [DataMember] attribute marking a field for WCF serialization",
                        "This code uses C# WCF to define a member field for inbound SOAP messages",
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
                        "C# MassTransit namespace import for message bus integration",
                        "This code uses C# MassTransit to interact with a message bus",
                    ),
                ),
                (
                    r"using NServiceBus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# NServiceBus namespace import for service bus integration",
                        "This code uses C# NServiceBus to interact with a service bus",
                    ),
                ),
                (
                    r"using Rebus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# Rebus namespace import for message bus integration",
                        "This code uses C# Rebus to interact with a message bus",
                    ),
                ),
                (
                    r"IConsumer<",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# IConsumer<T> interface implementing a message consumer",
                        "This code uses C# MassTransit to receive and consume inbound messages",
                    ),
                ),
                (
                    r"IMessageHandler",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# IMessageHandler interface handling inbound messages",
                        "This code uses C# messaging to handle incoming messages",
                    ),
                ),
                (
                    r"IBus\b",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "C# IBus interface sending messages via a message bus",
                        "This code uses C# messaging to send outbound messages to a bus",
                    ),
                ),
                (
                    r"Amazon\.SQS",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# Amazon.SQS client interacting with AWS Simple Queue Service",
                        "This code uses C# AWS SDK to interact with an SQS message queue",
                    ),
                ),
                (
                    r"Amazon\.SimpleNotificationService",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# Amazon.SimpleNotificationService client interacting with AWS SNS",
                        "This code uses C# AWS SDK to interact with an SNS notification topic",
                    ),
                ),
                (
                    r"Azure\.Messaging\.ServiceBus",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# Azure.Messaging.ServiceBus client interacting with Azure Service Bus",
                        "This code uses C# Azure SDK to interact with an Azure Service Bus",
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
                        "C# Microsoft.AspNetCore.SignalR namespace import for real-time communication",
                        "This code uses C# SignalR to accept real-time WebSocket connections",
                    ),
                ),
                (
                    r": Hub\b",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# Hub class defining a SignalR hub for real-time clients",
                        "This code uses C# SignalR to expose an inbound real-time communication hub",
                    ),
                ),
                (
                    r"HubConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# HubConnection class connecting to a remote SignalR hub",
                        "This code uses C# SignalR to connect to an outbound real-time hub",
                    ),
                ),
                (
                    r"TcpClient",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "C# TcpClient class opening an outbound TCP connection",
                        "This code uses C# networking to connect to an outbound TCP endpoint",
                    ),
                ),
                (
                    r"TcpListener",
                    Confidence.MEDIUM,
                    SignalDirection.INWARD,
                    (
                        "C# TcpListener class listening for inbound TCP connections",
                        "This code uses C# networking to accept inbound TCP connections",
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
                        "C# Entity Framework namespace import for ORM database access",
                        "This code uses C# Entity Framework to interact with a database",
                    ),
                ),
                (
                    r": DbContext",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Entity Framework DbContext subclass managing database session",
                        "This code uses C# Entity Framework to interact with a database",
                    ),
                ),
                (
                    r"DbSet<",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Entity Framework DbSet<T> property representing a database table",
                        "This code uses C# Entity Framework to query a database table",
                    ),
                ),
                (
                    r"\[Table\(",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# [Table] attribute mapping a class to a database table",
                        "This code uses C# Entity Framework to write to a mapped database table",
                    ),
                ),
                (
                    r"\[Key\]",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "C# [Key] attribute marking a property as a database primary key",
                        "This code uses C# Entity Framework to define a database primary key",
                    ),
                ),
                (
                    r"\[Column\(",
                    Confidence.MEDIUM,
                    SignalDirection.OUTWARD,
                    (
                        "C# [Column] attribute mapping a property to a database column",
                        "This code uses C# Entity Framework to write to a mapped database column",
                    ),
                ),
                (
                    r"SqlConnection",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# SqlConnection class opening a connection to a SQL Server database",
                        "This code uses C# ADO.NET to connect to an outbound SQL Server database",
                    ),
                ),
                (
                    r"using Dapper",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Dapper namespace import for lightweight ORM database access",
                        "This code uses C# Dapper to query a database",
                    ),
                ),
                (
                    r"Amazon\.DynamoDBv2",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Amazon.DynamoDBv2 client interacting with AWS DynamoDB",
                        "This code uses C# AWS SDK to write to a DynamoDB database",
                    ),
                ),
                (
                    r"Microsoft\.Azure\.Cosmos",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Microsoft.Azure.Cosmos client interacting with Azure Cosmos DB",
                        "This code uses C# Azure SDK to query an Azure Cosmos DB database",
                    ),
                ),
                (
                    r"using Neo4j\.Driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Neo4j.Driver namespace import for graph database access",
                        "This code uses C# Neo4j driver to connect to a Neo4j graph database",
                    ),
                ),
                (
                    r"IDriver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# IDriver interface for the Neo4j graph database driver",
                        "This code uses C# Neo4j driver to connect to a Neo4j graph database",
                    ),
                ),
                (
                    r"GraphDatabase\.Driver",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# GraphDatabase.Driver factory creating a Neo4j driver instance",
                        "This code uses C# Neo4j driver to connect to a Neo4j graph database",
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
                        "C# File.Read method reading data from the filesystem",
                        "This code uses C# System.IO to read from a file",
                    ),
                ),
                (
                    r"File\.Write",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# File.Write method writing data to the filesystem",
                        "This code uses C# System.IO to write to a file",
                    ),
                ),
                (
                    r"StreamReader",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# StreamReader class reading character data from a stream",
                        "This code uses C# System.IO to read from a stream or file",
                    ),
                ),
                (
                    r"StreamWriter",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# StreamWriter class writing character data to a stream",
                        "This code uses C# System.IO to write to a stream or file",
                    ),
                ),
                (
                    r"AWSSDK\.S3",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# AWSSDK.S3 client interacting with AWS S3 object storage",
                        "This code uses C# AWS SDK to write to an S3 storage bucket",
                    ),
                ),
                (
                    r"Azure\.Storage\.Blobs",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Azure.Storage.Blobs client interacting with Azure Blob Storage",
                        "This code uses C# Azure SDK to write to Azure Blob Storage",
                    ),
                ),
                (
                    r"Google\.Cloud\.Storage",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Google.Cloud.Storage client interacting with Google Cloud Storage",
                        "This code uses C# Google Cloud SDK to write to Google Cloud Storage",
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
                        "C# Grpc.Core library providing core gRPC primitives",
                        "This code uses C# gRPC to interact with a gRPC service",
                    ),
                ),
                (
                    r"Grpc\.Net\.Client",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# Grpc.Net.Client library creating a managed gRPC client",
                        "This code uses C# gRPC to call an outbound gRPC service",
                    ),
                ),
                (
                    r"ServerServiceDefinition",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# ServerServiceDefinition class registering a gRPC service implementation",
                        "This code uses C# gRPC to expose an inbound gRPC service",
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
                        "C# HotChocolate library exposing a GraphQL server",
                        "This code uses C# HotChocolate to expose an inbound GraphQL API",
                    ),
                ),
                (
                    r"GraphQL\.Server",
                    Confidence.HIGH,
                    SignalDirection.INWARD,
                    (
                        "C# GraphQL.Server library hosting a GraphQL endpoint",
                        "This code uses C# GraphQL.Server to expose an inbound GraphQL API",
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
                        "C# System.Net.Mail namespace providing SMTP email sending",
                        "This code uses C# System.Net.Mail to send outbound email messages",
                    ),
                ),
                (
                    r"SmtpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# SmtpClient class sending email via an SMTP server",
                        "This code uses C# SMTP to send outbound email messages",
                    ),
                ),
                (
                    r"MailKit",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# MailKit library sending and receiving email messages",
                        "This code uses C# MailKit to send outbound email messages",
                    ),
                ),
                (
                    r"MimeKit",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# MimeKit library constructing MIME email messages",
                        "This code uses C# MimeKit to produce outbound MIME email messages",
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
                        "C# IDistributedCache interface accessing a distributed cache",
                        "This code uses C# distributed caching to write to a remote cache store",
                    ),
                ),
                (
                    r"IMemoryCache",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# IMemoryCache interface accessing an in-process memory cache",
                        "This code uses C# memory caching to write to an in-process cache",
                    ),
                ),
                (
                    r"StackExchange\.Redis",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# StackExchange.Redis client connecting to a Redis cache",
                        "This code uses C# StackExchange.Redis to write to a Redis cache",
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
                        "C# IAsyncEnumerable<T> interface streaming data asynchronously",
                        "This code uses C# async streaming to expose an inbound data stream",
                    ),
                ),
                (
                    r"text/event-stream",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# text/event-stream content type enabling Server-Sent Events",
                        "This code uses C# HTTP to interact with a Server-Sent Events stream",
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
                        "C# FtpWebRequest class making outbound FTP requests",
                        "This code uses C# networking to connect to an outbound FTP server",
                    ),
                ),
                (
                    r"SftpClient",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# SftpClient class connecting to an outbound SFTP server",
                        "This code uses C# SSH.NET to connect to an outbound SFTP server",
                    ),
                ),
                (
                    r"FluentFTP",
                    Confidence.HIGH,
                    SignalDirection.OUTWARD,
                    (
                        "C# FluentFTP library connecting to an outbound FTP server",
                        "This code uses C# FluentFTP to connect to an outbound FTP server",
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
                        "C# Hangfire library scheduling background jobs",
                        "This code uses C# Hangfire to integrate with a background job scheduler",
                    ),
                ),
                (
                    r"Quartz",
                    Confidence.HIGH,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# Quartz.NET library scheduling recurring jobs",
                        "This code uses C# Quartz.NET to integrate with a job scheduling system",
                    ),
                ),
                (
                    r"IHostedService",
                    Confidence.MEDIUM,
                    SignalDirection.AMBIGUOUS,
                    (
                        "C# IHostedService interface running a background hosted service",
                        "This code uses C# hosting to integrate with a background service lifecycle",
                    ),
                ),
            ],
        },
    },
)
