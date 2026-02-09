"""ML-based code line classifier using LLM models."""

from .types import (
    ClassifiedLine,
    CompletionResult,
    FileClassification,
    MLIntegrationType,
    RepositoryClassification,
)
from .model_protocol import LineClassifierModel
from .classifier import classify_file, classify_repository

__all__ = [
    "ClassifiedLine",
    "CompletionResult",
    "FileClassification",
    "LineClassifierModel",
    "MLIntegrationType",
    "RepositoryClassification",
    "classify_file",
    "classify_repository",
]
