"""LLM-based integration signal concretisation via a local Ollama model.

For each unique file that contains FILE_CONTENT integration signals, this
module reads the full file, annotates it with the detected signal lines, and
asks a local Ollama model to classify each signal as DEFINITE_INWARD,
DEFINITE_OUTWARD, or NOT_DEFINITE.  The result is a ConcretisationResult
identical in shape to the ML-based one so callers can use either approach.

Usage (standalone):
    from repo_surveyor.integration_concretiser.ollama_concretiser import (
        concretise_with_ollama,
    )
    result = concretise_with_ollama(detector_result, model="qwen2.5-coder:7b-instruct")
"""

import json
import logging
from collections import defaultdict
from collections.abc import Callable
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

from repo_surveyor.detection.integration_detector import (
    EntityType,
    IntegrationDetectorResult,
)
from repo_surveyor.integration_concretiser.grouper import group_signals_by_ast_context
from repo_surveyor.integration_concretiser.llm_shared import (
    MAX_FILE_CHARS,
    SYSTEM_PROMPT,
    build_classification_prompt,
    parse_classification_response,
    read_file_bytes,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisedSignal,
    ConcretisationResult,
    SignalLike,
)
from repo_surveyor.training.types import TrainingLabel

logger = logging.getLogger(__name__)

_DEFAULT_OLLAMA_BASE_URL = "http://localhost:11434"
_DEFAULT_MODEL = "qwen2.5-coder:7b-instruct"


def _call_ollama(
    prompt: str,
    model: str,
    base_url: str,
) -> dict:
    """POST to the Ollama /api/generate endpoint and return the parsed response."""
    url = f"{base_url}/api/generate"
    payload = json.dumps(
        {
            "model": model,
            "system": SYSTEM_PROMPT,
            "prompt": prompt,
            "format": "json",
            "stream": False,
        }
    ).encode("utf-8")
    logger.debug("POST %s  model=%s  prompt_len=%d", url, model, len(prompt))
    req = Request(url, data=payload, headers={"Content-Type": "application/json"})
    try:
        with urlopen(req, timeout=180) as resp:
            raw = json.loads(resp.read())
            logger.debug(
                "Ollama response: done=%s  eval_count=%s",
                raw.get("done"),
                raw.get("eval_count"),
            )
            return raw
    except HTTPError as exc:
        logger.error("Ollama HTTP %d: %s", exc.code, exc.reason)
        return {}
    except URLError as exc:
        logger.error("Ollama connection error: %s", exc)
        return {}


def _parse_ollama_response(raw: dict) -> dict[int, tuple[TrainingLabel, float, str]]:
    """Parse an Ollama response into a {line_number: (label, confidence, reason)} map."""
    response_text = raw.get("response", "{}")
    logger.debug("Raw Ollama response text: %s", response_text[:500])
    try:
        data = json.loads(response_text)
    except json.JSONDecodeError as exc:
        logger.warning("Failed to JSON-decode Ollama response: %s", exc)
        return {}

    return parse_classification_response(data)


def _concretise_file(
    file_path: str,
    signals: list[SignalLike],
    signal_to_ast: dict[tuple[str, int], ASTContext],
    model: str,
    base_url: str,
    file_reader: Callable[[str], bytes],
) -> tuple[list[ConcretisedSignal], dict[tuple[str, int], dict]]:
    """Run Ollama classification for all signals in a single file."""
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

    raw_response = _call_ollama(prompt, model, base_url)
    line_map = _parse_ollama_response(raw_response)

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
                "  Line %4d  NOT in Ollama response — defaulting NOT_DEFINITE  [%s]  %s",
                ln,
                sig.integration_type.value,
                sig.match.line_content.strip()[:60],
            )

        concretised.append(
            ConcretisedSignal(original_signal=sig, ast_context=ast_ctx, label=label)
        )

    return concretised, metadata


def concretise_with_ollama(
    detector_result: IntegrationDetectorResult,
    model: str = _DEFAULT_MODEL,
    base_url: str = _DEFAULT_OLLAMA_BASE_URL,
    file_reader: Callable[[str], bytes] = read_file_bytes,
) -> tuple[ConcretisationResult, dict[tuple[str, int], dict]]:
    """Concretise FILE_CONTENT integration signals using a local Ollama LLM.

    For each unique file that contains signals, reads the file and asks the
    Ollama model to classify each signal line as DEFINITE_INWARD,
    DEFINITE_OUTWARD, or NOT_DEFINITE.  Returns a ConcretisationResult with
    the same shape as the ML-based concretiser.

    Args:
        detector_result: Output from the integration detector.
        model: Ollama model name (default: qwen2.5-coder:7b-instruct).
        base_url: Ollama API base URL (default: http://localhost:11434).
        file_reader: Callable that reads a file path to bytes (injectable for testing).

    Returns:
        Tuple of (ConcretisationResult, metadata) where metadata is a dict
        mapping (file_path, line_number) to {"confidence": float|None, "reason": str|None}.
    """
    file_content_signals: list[SignalLike] = [
        s
        for s in detector_result.integration_points
        if s.entity_type == EntityType.FILE_CONTENT
    ]

    logger.info(
        "Ollama concretiser: %d FILE_CONTENT signals across detector result",
        len(file_content_signals),
    )

    # Run AST grouping to get ASTContext for every signal.
    logger.info("Running AST walk-up grouping...")
    groups = group_signals_by_ast_context(file_content_signals, file_reader)
    logger.info("AST grouping produced %d groups", len(groups))

    # Build a flat (file_path, line_number) -> ASTContext lookup.
    signal_to_ast: dict[tuple[str, int], ASTContext] = {
        (group.file_path, sig.match.line_number): group.ast_context
        for group in groups
        for sig in group.signals
    }

    # Group signals by unique file.
    by_file: dict[str, list[SignalLike]] = defaultdict(list)
    for sig in file_content_signals:
        by_file[sig.match.file_path].append(sig)

    unique_files = sorted(by_file)
    logger.info(
        "Unique files to process: %d  (model=%s  base_url=%s)",
        len(unique_files),
        model,
        base_url,
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
            file_path, signals, signal_to_ast, model, base_url, file_reader
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
        "Ollama concretisation complete: submitted=%d  definite=%d  discarded=%d",
        result.signals_submitted,
        result.signals_definite,
        result.signals_discarded,
    )
    return result, all_metadata
