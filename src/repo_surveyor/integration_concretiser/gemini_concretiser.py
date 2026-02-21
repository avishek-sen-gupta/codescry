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
    SYSTEM_PROMPT_BATCHED,
    build_batched_classification_prompt,
    build_classification_prompt,
    parse_batched_classification_response,
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

_DEFAULT_MODEL = "gemini-2.5-flash"
_MAX_BATCH_PROMPT_CHARS = 80_000


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

    return _apply_line_map(file_path, signals, line_map, signal_to_ast)


def _apply_line_map(
    file_path: str,
    signals: list[IntegrationSignal],
    line_map: dict[int, tuple[TrainingLabel, float, str]],
    signal_to_ast: dict[tuple[str, int], ASTContext],
) -> tuple[list[ConcretisedSignal], dict[tuple[str, int], dict]]:
    """Apply a line-level classification map to signals, producing concretised results."""
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
            metadata[(sig.match.file_path, ln)] = {
                "confidence": None,
                "reason": None,
            }
            logger.warning(
                "  Line %4d  NOT in LLM response — defaulting NOT_DEFINITE  [%s]  %s",
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

    # --- Read all files and prepare (path, content, signals, truncated) ---
    file_data: list[tuple[str, str, list[IntegrationSignal], bool]] = []
    oversized_files: list[str] = []
    unreadable: dict[str, list[IntegrationSignal]] = {}

    for fp in unique_files:
        try:
            raw_bytes = file_reader(fp)
            content = raw_bytes.decode("utf-8", errors="replace")
        except OSError as exc:
            logger.error("Cannot read %s: %s", fp, exc)
            unreadable[fp] = by_file[fp]
            continue
        truncated = len(content) > MAX_FILE_CHARS
        if truncated:
            content = content[:MAX_FILE_CHARS]
        file_data.append((fp, content, by_file[fp], truncated))

    # --- Partition into solo (oversized) and batchable files ---
    solo: list[tuple[str, str, list[IntegrationSignal], bool]] = []
    batchable: list[tuple[str, str, list[IntegrationSignal], bool]] = []
    for entry in file_data:
        fp, content, sigs, trunc = entry
        prompt_estimate = len(build_classification_prompt(fp, content, sigs, trunc))
        if prompt_estimate > _MAX_BATCH_PROMPT_CHARS:
            solo.append(entry)
        else:
            batchable.append(entry)

    # --- Form batches from batchable files ---
    batches: list[list[tuple[str, str, list[IntegrationSignal], bool]]] = []
    current_batch: list[tuple[str, str, list[IntegrationSignal], bool]] = []
    current_chars = 0
    for entry in batchable:
        fp, content, sigs, trunc = entry
        entry_chars = len(build_classification_prompt(fp, content, sigs, trunc))
        if current_batch and current_chars + entry_chars > _MAX_BATCH_PROMPT_CHARS:
            batches.append(current_batch)
            current_batch = []
            current_chars = 0
        current_batch.append(entry)
        current_chars += entry_chars
    if current_batch:
        batches.append(current_batch)

    logger.info(
        "Batch plan: %d solo files, %d batches (%d batchable files), %d unreadable",
        len(solo),
        len(batches),
        len(batchable),
        len(unreadable),
    )

    all_concretised: list[ConcretisedSignal] = []
    all_metadata: dict[tuple[str, int], dict] = {}
    call_idx = 0
    total_calls = len(solo) + len(batches)

    # --- Process solo (oversized) files ---
    for fp, content, sigs, trunc in solo:
        call_idx += 1
        logger.info(
            "[%d/%d] Solo file: %s  (%d signals)", call_idx, total_calls, fp, len(sigs)
        )
        prompt = build_classification_prompt(fp, content, sigs, trunc)
        data = client.classify(SYSTEM_PROMPT, prompt)
        line_map = parse_classification_response(data)
        file_concretised, file_metadata = _apply_line_map(
            fp, sigs, line_map, signal_to_ast
        )
        all_concretised.extend(file_concretised)
        all_metadata.update(file_metadata)

    # --- Process batched files ---
    for batch in batches:
        call_idx += 1
        batch_paths = [fp for fp, _, _, _ in batch]
        total_sigs = sum(len(sigs) for _, _, sigs, _ in batch)
        logger.info(
            "[%d/%d] Batch of %d files  (%d signals): %s",
            call_idx,
            total_calls,
            len(batch),
            total_sigs,
            ", ".join(batch_paths),
        )
        if len(batch) == 1:
            fp, content, sigs, trunc = batch[0]
            prompt = build_classification_prompt(fp, content, sigs, trunc)
            data = client.classify(SYSTEM_PROMPT, prompt)
            line_map = parse_classification_response(data)
            file_concretised, file_metadata = _apply_line_map(
                fp, sigs, line_map, signal_to_ast
            )
            all_concretised.extend(file_concretised)
            all_metadata.update(file_metadata)
        else:
            prompt = build_batched_classification_prompt(batch)
            data = client.classify(SYSTEM_PROMPT_BATCHED, prompt)
            batched_map = parse_batched_classification_response(data)
            for fp, _, sigs, _ in batch:
                line_map = batched_map.get(fp, {})
                file_concretised, file_metadata = _apply_line_map(
                    fp, sigs, line_map, signal_to_ast
                )
                all_concretised.extend(file_concretised)
                all_metadata.update(file_metadata)

    # --- Handle unreadable files ---
    for fp, sigs in unreadable.items():
        for sig in sigs:
            ast_ctx = signal_to_ast.get(
                (sig.match.file_path, sig.match.line_number),
                ASTContext("unknown", "", sig.match.line_number, sig.match.line_number),
            )
            all_concretised.append(
                ConcretisedSignal(
                    original_signal=sig,
                    ast_context=ast_ctx,
                    label=TrainingLabel.NOT_DEFINITE,
                )
            )

    definite = sum(1 for s in all_concretised if s.is_definite)
    result = ConcretisationResult(
        concretised=tuple(all_concretised),
        signals_submitted=len(all_concretised),
        signals_definite=definite,
        signals_discarded=len(all_concretised) - definite,
    )
    logger.info(
        "Gemini concretisation complete: submitted=%d  definite=%d  discarded=%d  api_calls=%d",
        result.signals_submitted,
        result.signals_definite,
        result.signals_discarded,
        total_calls,
    )
    return result, all_metadata
