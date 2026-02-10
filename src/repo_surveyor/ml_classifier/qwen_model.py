"""Concrete LineClassifierModel implementation using Qwen2.5-Coder via Ollama."""

from .types import CompletionResult

_MODEL = "qwen2.5-coder:1.5b-instruct"


class QwenClassifierModel:
    """Qwen2.5-Coder-1.5B-Instruct model served by Ollama."""

    def __init__(self, model: str = _MODEL) -> None:
        import ollama  # type: ignore[import-not-found]

        self._client = ollama.Client()
        self._model = model

    @property
    def model_id(self) -> str:
        return self._model

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        response = self._client.chat(
            model=self._model,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            options={"temperature": 0.0},
        )
        text = response.message.content or ""
        return CompletionResult(
            text=text,
            prompt_tokens=response.prompt_eval_count or 0,
            completion_tokens=response.eval_count or 0,
        )
