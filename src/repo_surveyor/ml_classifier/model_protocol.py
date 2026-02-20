"""Protocol defining the model interface for dependency injection."""

from typing import Protocol

from repo_surveyor.ml_classifier.types import CompletionResult


class LLMModel(Protocol):
    """Protocol for LLM-backed models.

    Implementations must provide a classify method that accepts
    system and user prompts and returns a CompletionResult.
    """

    @property
    def model_id(self) -> str: ...

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult: ...
