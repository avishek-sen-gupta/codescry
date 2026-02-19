"""Concrete LLMModel implementation using Claude via the Anthropic API."""

import logging
import time
from dataclasses import dataclass

from .types import CompletionResult

logger = logging.getLogger(__name__)


class _Defaults:
    """Default configuration for Claude model."""

    MODEL = "claude-sonnet-4-20250514"
    MAX_TOKENS = 4096
    TEMPERATURE = 0.0
    BATCH_POLL_INITIAL_DELAY_SECONDS = 30
    BATCH_POLL_MAX_DELAY_SECONDS = 300
    BATCH_POLL_BACKOFF_FACTOR = 1.5


@dataclass(frozen=True)
class BatchStatus:
    """Snapshot of a batch's processing status."""

    batch_id: str
    processing_status: str
    succeeded: int
    errored: int
    expired: int
    processing: int
    canceled: int


@dataclass(frozen=True)
class BatchResult:
    """Result of a single request within a completed batch."""

    custom_id: str
    completion: CompletionResult
    succeeded: bool


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

    def create_batch(
        self,
        requests: list[tuple[str, str, str]],
        temperature: float = _Defaults.TEMPERATURE,
    ) -> str:
        """Submit a batch of requests to the Anthropic Batches API.

        Args:
            requests: List of (custom_id, system_prompt, user_prompt) tuples.
            temperature: Temperature for generation.

        Returns:
            The batch ID for polling and result retrieval.
        """
        from anthropic.types.message_create_params import (
            MessageCreateParamsNonStreaming,
        )
        from anthropic.types.messages.batch_create_params import Request

        batch_requests = [
            Request(
                custom_id=custom_id,
                params=MessageCreateParamsNonStreaming(
                    model=self._model,
                    max_tokens=self._max_tokens,
                    temperature=temperature,
                    system=system_prompt,
                    messages=[{"role": "user", "content": user_prompt}],
                ),
            )
            for custom_id, system_prompt, user_prompt in requests
        ]

        batch = self._client.messages.batches.create(requests=batch_requests)
        logger.info("Created batch %s with %d requests", batch.id, len(requests))
        return batch.id

    def poll_batch(self, batch_id: str) -> None:
        """Poll a batch until it reaches 'ended' status.

        Uses exponential backoff between polls.

        Args:
            batch_id: The batch ID returned by create_batch().
        """
        delay = _Defaults.BATCH_POLL_INITIAL_DELAY_SECONDS

        while True:
            batch = self._client.messages.batches.retrieve(batch_id)
            if batch.processing_status == "ended":
                logger.info(
                    "Batch %s ended: %d succeeded, %d errored, %d expired",
                    batch_id,
                    batch.request_counts.succeeded,
                    batch.request_counts.errored,
                    batch.request_counts.expired,
                )
                return

            logger.info(
                "Batch %s status: %s (processing: %d)",
                batch_id,
                batch.processing_status,
                batch.request_counts.processing,
            )
            time.sleep(delay)
            delay = min(
                delay * _Defaults.BATCH_POLL_BACKOFF_FACTOR,
                _Defaults.BATCH_POLL_MAX_DELAY_SECONDS,
            )

    def batch_status(self, batch_id: str) -> BatchStatus:
        """Query the current status of a batch without blocking.

        Args:
            batch_id: The batch ID to check.

        Returns:
            BatchStatus with current processing counts.
        """
        batch = self._client.messages.batches.retrieve(batch_id)
        return BatchStatus(
            batch_id=batch_id,
            processing_status=batch.processing_status,
            succeeded=batch.request_counts.succeeded,
            errored=batch.request_counts.errored,
            expired=batch.request_counts.expired,
            processing=batch.request_counts.processing,
            canceled=batch.request_counts.canceled,
        )

    def retrieve_batch_results(self, batch_id: str) -> dict[str, BatchResult]:
        """Retrieve results from a completed batch.

        Args:
            batch_id: The batch ID of a completed batch.

        Returns:
            Dict mapping custom_id to BatchResult.
        """
        results: dict[str, BatchResult] = {}

        for entry in self._client.messages.batches.results(batch_id):
            custom_id = entry.custom_id

            if entry.result.type == "succeeded":
                message = entry.result.message
                text = message.content[0].text if message.content else ""
                results[custom_id] = BatchResult(
                    custom_id=custom_id,
                    completion=CompletionResult(
                        text=text,
                        prompt_tokens=message.usage.input_tokens,
                        completion_tokens=message.usage.output_tokens,
                    ),
                    succeeded=True,
                )
            else:
                logger.warning(
                    "Batch request %s failed: %s", custom_id, entry.result.type
                )
                results[custom_id] = BatchResult(
                    custom_id=custom_id,
                    completion=CompletionResult(
                        text="", prompt_tokens=0, completion_tokens=0
                    ),
                    succeeded=False,
                )

        return results
