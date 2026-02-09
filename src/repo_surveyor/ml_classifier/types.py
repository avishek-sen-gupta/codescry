"""Types for ML-based code line classification."""

from dataclasses import dataclass
from enum import Enum


class MLIntegrationType(Enum):
    """Granular integration types detected by ML classification."""

    HTTP_CLIENT = "http_client"
    HTTP_SERVER = "http_server"
    REST_ENDPOINT = "rest_endpoint"
    GRAPHQL = "graphql"
    GRPC = "grpc"
    WEBSOCKET = "websocket"
    SOAP = "soap"
    TCP_SOCKET = "tcp_socket"
    UDP_SOCKET = "udp_socket"
    DATABASE_QUERY = "database_query"
    DATABASE_CONNECTION = "database_connection"
    ORM = "orm"
    CACHE = "cache"
    MESSAGE_PUBLISH = "message_publish"
    MESSAGE_SUBSCRIBE = "message_subscribe"
    STREAM_PRODUCER = "stream_producer"
    STREAM_CONSUMER = "stream_consumer"
    FILE_IO = "file_io"
    PROCESS_EXEC = "process_exec"
    FFI = "ffi"
    NONE = "none"


@dataclass(frozen=True)
class CompletionResult:
    """Result from a model completion call."""

    text: str
    prompt_tokens: int
    completion_tokens: int


@dataclass(frozen=True)
class ClassifiedLine:
    """A single source line with its ML classification."""

    line_number: int
    line_content: str
    integration_type: MLIntegrationType
    raw_model_label: str


@dataclass(frozen=True)
class FileClassification:
    """Classification result for a single file."""

    file_path: str
    language: str | None
    classified_lines: tuple[ClassifiedLine, ...]
    lines_submitted: int
    lines_skipped: int


@dataclass(frozen=True)
class RepositoryClassification:
    """Classification result for an entire repository."""

    file_classifications: tuple[FileClassification, ...]
    files_scanned: int
    model_id: str
