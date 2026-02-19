"""Concrete LLMModel implementation using Claude via the Anthropic API."""

from .types import CompletionResult


class _Defaults:
    """Default configuration for Claude model."""

    MODEL = "claude-sonnet-4-20250514"
    MAX_TOKENS = 4096
    TEMPERATURE = 0.0


class ClaudeClassifierModel:
    """Claude model served by the Anthropic API.

    Requires the ANTHROPIC_API_KEY environment variable to be set,
    or an explicit api_key passed to the constructor.
    """

    def __init__(
        self,
        model: str = _Defaults.MODEL,
        max_tokens: int = _Defaults.MAX_TOKENS,
        api_key: str = "",
    ) -> None:
        import anthropic  # type: ignore[import-not-found]

        self._client = (
            anthropic.Anthropic(api_key=api_key) if api_key else anthropic.Anthropic()
        )
        self._model = model
        self._max_tokens = max_tokens

    @property
    def model_id(self) -> str:
        return self._model

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        response = self._client.messages.create(
            model=self._model,
            max_tokens=self._max_tokens,
            temperature=_Defaults.TEMPERATURE,
            system=system_prompt,
            messages=[{"role": "user", "content": user_prompt}],
        )
        text = response.content[0].text if response.content else ""
        return CompletionResult(
            text=text,
            prompt_tokens=response.usage.input_tokens,
            completion_tokens=response.usage.output_tokens,
        )
