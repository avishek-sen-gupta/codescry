"""Shared types for integration patterns."""

from enum import Enum


class Language(Enum):
    """Supported programming languages for integration detection."""

    JAVA = "Java"
    RUST = "Rust"
    PYTHON = "Python"
    TYPESCRIPT = "TypeScript"
    JAVASCRIPT = "JavaScript"
    GO = "Go"
    CSHARP = "C#"
    KOTLIN = "Kotlin"
    SCALA = "Scala"
    RUBY = "Ruby"
    PHP = "PHP"
    COBOL = "COBOL"
    PLI = "PL/I"


class IntegrationType(Enum):
    """Types of system integrations that can be detected."""

    HTTP_REST = "http_rest"
    SOAP = "soap"
    MESSAGING = "messaging"
    SOCKET = "socket"
    DATABASE = "database"
    FILE_IO = "file_io"
    GRPC = "grpc"


class Confidence(Enum):
    """Confidence levels for integration point detection."""

    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"
