"""Protocol defining the model interface for dependency injection."""

from typing import Protocol

from .types import CompletionResult


class LineClassifierModel(Protocol):
    """Protocol for models that classify code lines.

    Implementations must provide a classify method that accepts
    system and user prompts and returns a CompletionResult.
    """

    @property
    def model_id(self) -> str: ...

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult: ...
