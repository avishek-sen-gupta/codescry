"""Concrete LineClassifierModel implementation using Qwen2.5-Coder via llama-cpp-python."""

from .types import CompletionResult

_REPO_ID = "Qwen/Qwen2.5-Coder-0.5B-Instruct-GGUF"
_FILENAME = "qwen2.5-coder-0.5b-instruct-q8_0.gguf"


class QwenClassifierModel:
    """Qwen2.5-Coder-0.5B-Instruct model loaded via llama-cpp-python.

    Downloads the GGUF file from HuggingFace Hub on first use.
    """

    def __init__(
        self,
        n_ctx: int = 4096,
        n_gpu_layers: int = -1,
        verbose: bool = False,
    ) -> None:
        from huggingface_hub import hf_hub_download  # type: ignore[import-not-found]
        from llama_cpp import Llama  # type: ignore[import-not-found]

        model_path = hf_hub_download(repo_id=_REPO_ID, filename=_FILENAME)
        self._llm = Llama(
            model_path=model_path,
            n_ctx=n_ctx,
            n_gpu_layers=n_gpu_layers,
            verbose=verbose,
        )
        self._model_id = f"{_REPO_ID}/{_FILENAME}"

    @property
    def model_id(self) -> str:
        return self._model_id

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        response = self._llm.create_chat_completion(
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            temperature=0.0,
            max_tokens=2048,
        )
        text = response["choices"][0]["message"]["content"] or ""
        usage = response.get("usage", {})
        return CompletionResult(
            text=text,
            prompt_tokens=usage.get("prompt_tokens", 0),
            completion_tokens=usage.get("completion_tokens", 0),
        )
