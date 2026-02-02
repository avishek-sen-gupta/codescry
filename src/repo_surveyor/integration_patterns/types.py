"""Shared types for integration patterns."""

from enum import Enum


class IntegrationType(Enum):
    """Types of system integrations that can be detected."""

    HTTP_REST = "http_rest"
    SOAP = "soap"
    MESSAGING = "messaging"
    SOCKET = "socket"
    DATABASE = "database"


class Confidence(Enum):
    """Confidence levels for integration point detection."""

    HIGH = "high"
    MEDIUM = "medium"
    LOW = "low"
