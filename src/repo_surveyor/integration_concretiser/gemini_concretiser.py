"""LLM-based integration signal concretisation via Google Gemini Flash.

Mirrors the Ollama concretiser pattern (file-level batching, same system prompt,
same JSON response shape) but calls the Gemini Flash API via the ``google-genai``
SDK.  Combines LLM reasoning with cloud-speed inference for classification.

Usage:
    from repo_surveyor.integration_concretiser.gemini_concretiser import (
        concretise_with_gemini,
    )
    result, metadata = concretise_with_gemini(detector_result, api_key="...")
"""

import json
import logging
import time
from collections import defaultdict
from collections.abc import Callable

from repo_surveyor.detection.integration_detector import (
    EntityType,
    IntegrationDetectorResult,
    IntegrationSignal,
)
from repo_surveyor.integration_concretiser.grouper import group_signals_by_ast_context
from repo_surveyor.integration_concretiser.llm_shared import (
    INITIAL_BACKOFF_SECONDS,
    MAX_FILE_CHARS,
    MAX_RETRIES,
    SYSTEM_PROMPT,
    build_classification_prompt,
    parse_classification_response,
    read_file_bytes,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisedSignal,
    ConcretisationResult,
)
from repo_surveyor.training.types import TrainingLabel

logger = logging.getLogger(__name__)

_DEFAULT_MODEL = "gemini-2.0-flash"


class GeminiClassificationClient:
    """Thin wrapper over the Google GenAI SDK for classification calls."""

    def __init__(self, api_key: str, model: str = _DEFAULT_MODEL) -> None:
        from google import genai

        self._client = genai.Client(api_key=api_key)
        self._model = model

    def classify(self, system_prompt: str, user_prompt: str) -> dict:
        """Call Gemini to classify signals, returning parsed JSON.

        Retries with exponential backoff on 429 / RESOURCE_EXHAUSTED errors.
        """
        from google.genai import types

        logger.debug(
            "[Gemini] classify  model=%s  prompt_len=%d",
            self._model,
            len(user_prompt),
        )
        backoff = INITIAL_BACKOFF_SECONDS
        for attempt in range(MAX_RETRIES):
            try:
                t0 = time.monotonic()
                response = self._client.models.generate_content(
                    model=self._model,
                    contents=user_prompt,
                    config=types.GenerateContentConfig(
                        system_instruction=system_prompt,
                        response_mime_type="application/json",
                    ),
                )
                elapsed = time.monotonic() - t0
                logger.debug(
                    "[Gemini] Response received in %.2fs (model=%s)",
                    elapsed,
                    self._model,
                )
                raw_text = response.text or "{}"
                logger.debug("[Gemini] Raw response text: %s", raw_text[:500])
                return json.loads(raw_text)
            except json.JSONDecodeError as exc:
                logger.warning(
                    "[Gemini] Failed to JSON-decode response: %s  raw=%s",
                    exc,
                    (response.text or "")[:200],
                )
                return {}
            except Exception as exc:
                if "429" not in str(exc) and "RESOURCE_EXHAUSTED" not in str(exc):
                    raise
                logger.warning(
                    "[Gemini] Rate-limited (attempt %d/%d), retrying in %.1fs...",
                    attempt + 1,
                    MAX_RETRIES,
                    backoff,
                )
                time.sleep(backoff)
                backoff *= 2

        raise RuntimeError(
            f"[Gemini] Classification failed after {MAX_RETRIES} retries "
            "due to rate limiting"
        )


def _concretise_file(
    file_path: str,
    signals: list[IntegrationSignal],
    signal_to_ast: dict[tuple[str, int], ASTContext],
    client: GeminiClassificationClient,
    file_reader: Callable[[str], bytes],
) -> tuple[list[ConcretisedSignal], dict[tuple[str, int], dict]]:
    """Run Gemini classification for all signals in a single file."""
    logger.info("Processing file: %s  (%d signals)", file_path, len(signals))

    try:
        raw_bytes = file_reader(file_path)
        file_content = raw_bytes.decode("utf-8", errors="replace")
    except OSError as exc:
        logger.error("Cannot read %s: %s", file_path, exc)
        return [
            ConcretisedSignal(
                original_signal=sig,
                ast_context=signal_to_ast.get(
                    (sig.match.file_path, sig.match.line_number),
                    ASTContext(
                        "unknown", "", sig.match.line_number, sig.match.line_number
                    ),
                ),
                label=TrainingLabel.NOT_DEFINITE,
            )
            for sig in signals
        ], {}

    truncated = len(file_content) > MAX_FILE_CHARS
    if truncated:
        logger.debug(
            "File %s is %d chars — truncating to %d",
            file_path,
            len(file_content),
            MAX_FILE_CHARS,
        )
        file_content = file_content[:MAX_FILE_CHARS]

    prompt = build_classification_prompt(file_path, file_content, signals, truncated)
    logger.debug("Prompt length: %d chars", len(prompt))

    data = client.classify(SYSTEM_PROMPT, prompt)
    line_map = parse_classification_response(data)

    concretised: list[ConcretisedSignal] = []
    metadata: dict[tuple[str, int], dict] = {}
    for sig in signals:
        ln = sig.match.line_number
        ast_ctx = signal_to_ast.get(
            (sig.match.file_path, ln),
            ASTContext("unknown", "", ln, ln),
        )
        if ln in line_map:
            label, confidence, reason = line_map[ln]
            metadata[(sig.match.file_path, ln)] = {
                "confidence": confidence,
                "reason": reason,
            }
            logger.info(
                "  Line %4d  %-20s  conf=%.2f  [%s]  %s  → %s",
                ln,
                label.value,
                confidence,
                sig.integration_type.value,
                sig.match.line_content.strip()[:60],
                reason[:60],
            )
        else:
            label = TrainingLabel.NOT_DEFINITE
            metadata[(sig.match.file_path, ln)] = {"confidence": None, "reason": None}
            logger.warning(
                "  Line %4d  NOT in Gemini response — defaulting NOT_DEFINITE  [%s]  %s",
                ln,
                sig.integration_type.value,
                sig.match.line_content.strip()[:60],
            )

        concretised.append(
            ConcretisedSignal(original_signal=sig, ast_context=ast_ctx, label=label)
        )

    return concretised, metadata


def concretise_with_gemini(
    detector_result: IntegrationDetectorResult,
    api_key: str,
    model: str = _DEFAULT_MODEL,
    file_reader: Callable[[str], bytes] = read_file_bytes,
) -> tuple[ConcretisationResult, dict[tuple[str, int], dict]]:
    """Concretise FILE_CONTENT integration signals using Gemini Flash.

    For each unique file that contains signals, reads the file and asks the
    Gemini model to classify each signal line as DEFINITE_INWARD,
    DEFINITE_OUTWARD, or NOT_DEFINITE.

    Args:
        detector_result: Output from the integration detector.
        api_key: Google AI API key for the Gemini API.
        model: Gemini model name (default: gemini-2.0-flash).
        file_reader: Callable that reads a file path to bytes (injectable for testing).

    Returns:
        Tuple of (ConcretisationResult, metadata) where metadata is a dict
        mapping (file_path, line_number) to {"confidence": float|None, "reason": str|None}.
    """
    client = GeminiClassificationClient(api_key=api_key, model=model)

    file_content_signals: list[IntegrationSignal] = [
        s
        for s in detector_result.integration_points
        if s.entity_type == EntityType.FILE_CONTENT
    ]

    logger.info(
        "Gemini concretiser: %d FILE_CONTENT signals across detector result",
        len(file_content_signals),
    )

    logger.info("Running AST walk-up grouping...")
    groups = group_signals_by_ast_context(file_content_signals, file_reader)
    logger.info("AST grouping produced %d groups", len(groups))

    signal_to_ast: dict[tuple[str, int], ASTContext] = {
        (group.file_path, sig.match.line_number): group.ast_context
        for group in groups
        for sig in group.signals
    }

    by_file: dict[str, list[IntegrationSignal]] = defaultdict(list)
    for sig in file_content_signals:
        by_file[sig.match.file_path].append(sig)

    unique_files = sorted(by_file)
    logger.info(
        "Unique files to process: %d  (model=%s)",
        len(unique_files),
        model,
    )

    all_concretised: list[ConcretisedSignal] = []
    all_metadata: dict[tuple[str, int], dict] = {}
    for idx, file_path in enumerate(unique_files, 1):
        signals = by_file[file_path]
        logger.info(
            "[%d/%d] %s  (%d signals)",
            idx,
            len(unique_files),
            file_path,
            len(signals),
        )
        file_concretised, file_metadata = _concretise_file(
            file_path, signals, signal_to_ast, client, file_reader
        )
        all_concretised.extend(file_concretised)
        all_metadata.update(file_metadata)

    definite = sum(1 for s in all_concretised if s.is_definite)
    result = ConcretisationResult(
        concretised=tuple(all_concretised),
        signals_submitted=len(all_concretised),
        signals_definite=definite,
        signals_discarded=len(all_concretised) - definite,
    )
    logger.info(
        "Gemini concretisation complete: submitted=%d  definite=%d  discarded=%d",
        result.signals_submitted,
        result.signals_definite,
        result.signals_discarded,
    )
    return result, all_metadata
