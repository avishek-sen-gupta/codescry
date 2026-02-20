"""Shared types for integration patterns."""

from dataclasses import dataclass
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
    C = "C"
    CPP = "C++"
    COBOL = "COBOL"
    PLI = "PL/I"
    PASCAL = "Pascal"

    @classmethod
    def from_name(cls, name: str) -> "Language | None":
        """Look up a Language member by its value (display name).

        Args:
            name: The language display name, e.g. "Java", "C#", "PL/I".

        Returns:
            The matching Language member, or None if not found.
        """
        return next((member for member in cls if member.value == name), None)


class IntegrationType(Enum):
    """Types of system integrations that can be detected."""

    HTTP_REST = "http_rest"
    SOAP = "soap"
    MESSAGING = "messaging"
    SOCKET = "socket"
    DATABASE = "database"
    FILE_IO = "file_io"
    GRPC = "grpc"
    GRAPHQL = "graphql"
    EMAIL = "email"
    CACHING = "caching"
    SSE_STREAMING = "sse_streaming"
    SCHEDULING = "scheduling"
    FTP_SFTP = "ftp_sftp"


class Confidence(Enum):
    """Confidence levels for integration point detection."""

    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"


class SignalDirection(Enum):
    """Direction of a detected integration signal.

    INWARD  – the service exposes this integration point (receives external calls).
    OUTWARD – the service consumes an external system (makes outbound calls).
    AMBIGUOUS – the pattern alone cannot determine direction.
    """

    INWARD = "inward"
    OUTWARD = "outward"
    AMBIGUOUS = "ambiguous"


@dataclass(frozen=True)
class BasePatternSpec:
    """Base integration patterns for a language or common patterns."""

    patterns: dict


@dataclass(frozen=True)
class FrameworkPatternSpec:
    """Integration patterns for a specific framework."""

    name: str
    patterns: dict
    import_patterns: tuple[str, ...] = ()


class PatternKey:
    """Dict keys inside integration pattern dicts."""

    PATTERNS = "patterns"
    DIRECTORY_PATTERNS = "directory_patterns"
