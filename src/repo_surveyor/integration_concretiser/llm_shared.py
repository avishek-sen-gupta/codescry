"""Shared utilities for LLM-based integration signal concretisers.

Contains the system prompt, prompt builder, response parser, and file reader
used by both the Ollama and Gemini concretisers.
"""

import json
import logging
from collections import defaultdict

from repo_surveyor.detection.integration_detector import IntegrationSignal
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    CompositeIntegrationSignal,
    SignalLike,
    SignalValidity,
)
from repo_surveyor.integration_patterns import SignalDirection
from repo_surveyor.training.types import TrainingLabel

logger = logging.getLogger(__name__)

_LABEL_INSTRUCTIONS = """\
Valid labels:
  DEFINITE_INWARD   — the code exposes a service boundary to callers:
                       HTTP/REST/gRPC server endpoint, message consumer/listener,
                       socket server accept, SSE emitter, scheduled job entry point.
  DEFINITE_OUTWARD  — the code calls an external system:
                       HTTP client call, database connection/query, file write,
                       message publish/send, cache store, SMTP send, FTP upload.
  NOT_DEFINITE      — the code IS a real integration boundary, but the direction
                       (inward vs outward) cannot be determined from context alone.
  NOT_INTEGRATION   — the match is incidental and not a real integration boundary:
                       variable/field named 'file' or 'cache', string literal containing
                       a URL, test assertion, generated parser code, log statement,
                       import statement, or any other false positive."""

SYSTEM_PROMPT = f"""You are an expert code analyst specialising in detecting integration boundaries in software systems.

Given a source file and a list of lines flagged as potential integration signals, classify each signal line.

Respond ONLY with a JSON object — no prose, no markdown fences — in this exact shape:
{{
  "integrations": [
    {{"line": <int>, "label": "<LABEL>", "confidence": <float 0-1>, "reason": "<one sentence>"}},
    ...
  ]
}}

{_LABEL_INSTRUCTIONS}

Classify ALL signal lines listed. If a line number is not in the file or cannot be
determined, still include it with label NOT_INTEGRATION."""

SYSTEM_PROMPT_BATCHED = f"""You are an expert code analyst specialising in detecting integration boundaries in software systems.

You will be given MULTIPLE source files, each delimited by ===FILE: <path>=== markers.
For each file, a list of lines flagged as potential integration signals follows.

Respond ONLY with a JSON object — no prose, no markdown fences — in this exact shape:
{{
  "integrations": [
    {{"file": "<path>", "line": <int>, "label": "<LABEL>", "confidence": <float 0-1>, "reason": "<one sentence>"}},
    ...
  ]
}}

The "file" field MUST match the path shown in the ===FILE: <path>=== header exactly.

{_LABEL_INSTRUCTIONS}

Classify ALL signal lines listed across ALL files. If a line number is not in the file or cannot be
determined, still include it with label NOT_INTEGRATION."""

_LABEL_TO_VALIDITY_DIRECTION: dict[str, tuple[SignalValidity, SignalDirection]] = {
    "DEFINITE_INWARD": (SignalValidity.SIGNAL, SignalDirection.INWARD),
    "DEFINITE_OUTWARD": (SignalValidity.SIGNAL, SignalDirection.OUTWARD),
    "NOT_DEFINITE": (SignalValidity.SIGNAL, SignalDirection.AMBIGUOUS),
    "NOT_INTEGRATION": (SignalValidity.NOISE, SignalDirection.NOT_INTEGRATION),
}


def map_label_to_validity_direction(
    label_str: str,
) -> tuple[SignalValidity, SignalDirection]:
    """Map an LLM label string to a (validity, direction) pair."""
    result = _LABEL_TO_VALIDITY_DIRECTION.get(label_str)
    if result is not None:
        return result
    logger.warning(
        "Unknown label %r — defaulting to (NOISE, NOT_INTEGRATION)",
        label_str,
    )
    return (SignalValidity.NOISE, SignalDirection.NOT_INTEGRATION)


MAX_FILE_CHARS = 12_000

MAX_RETRIES = 5
INITIAL_BACKOFF_SECONDS = 2.0


def read_file_bytes(file_path: str) -> bytes:
    """Read a file and return its raw bytes."""
    with open(file_path, "rb") as f:
        return f.read()


def _merge_signal_lines(
    signals: list[SignalLike],
) -> list[tuple[int, str, str, str]]:
    """Deduplicate signals by (line_number, line_content), merging integration types.

    Returns sorted list of (line_number, merged_types_str, line_content, file_path).
    """
    types_by_line: dict[tuple[int, str], list[str]] = defaultdict(list)
    path_by_line: dict[tuple[int, str], str] = {}
    for s in signals:
        key = (s.match.line_number, s.match.line_content.strip())
        itype = s.integration_type.value
        if itype not in types_by_line[key]:
            types_by_line[key].append(itype)
        path_by_line[key] = s.match.file_path

    return sorted(
        (ln, ", ".join(types), content, path_by_line[(ln, content)])
        for (ln, content), types in types_by_line.items()
    )


def _build_signal_summary(
    signals: list[SignalLike],
    signal_to_ast: dict[tuple[str, int], ASTContext] = {},
) -> str:
    """Build the signal-lines block for a prompt.

    When ``signal_to_ast`` is provided, each signal line includes its
    enclosing AST statement context so the LLM can make better-informed
    classification decisions.
    """
    lines: list[str] = []
    for ln, types, content, file_path in _merge_signal_lines(signals):
        ast_ctx = signal_to_ast.get((file_path, ln))
        if ast_ctx and ast_ctx.node_text:
            lines.append(
                f"  Line {ln}: [{types}] {content}\n"
                f"    AST context ({ast_ctx.node_type}, lines {ast_ctx.start_line}-{ast_ctx.end_line}):\n"
                f"    ```\n    {ast_ctx.node_text}\n    ```"
            )
        else:
            lines.append(f"  Line {ln}: [{types}] {content}")
    return "\n".join(lines)


def _truncation_note(truncated: bool) -> str:
    if not truncated:
        return ""
    return (
        f"\n[NOTE: File truncated to first {MAX_FILE_CHARS} chars; "
        "some signal lines may not appear in the excerpt shown below]"
    )


def build_classification_prompt(
    file_path: str,
    file_content: str,
    signals: list[SignalLike],
    truncated: bool,
    signal_to_ast: dict[tuple[str, int], ASTContext] = {},
) -> str:
    """Build the user prompt for LLM-based classification of a single file."""
    signal_summary = _build_signal_summary(signals, signal_to_ast)
    return (
        f"File: {file_path}{_truncation_note(truncated)}\n\n"
        f"Detected signal lines to classify:\n{signal_summary}\n\n"
        f"File content:\n```\n{file_content}\n```\n\n"
        "Classify each signal line and return the JSON object."
    )


def build_batched_classification_prompt(
    files: list[tuple[str, str, list[SignalLike], bool]],
    signal_to_ast: dict[tuple[str, int], ASTContext] = {},
) -> str:
    """Build a batched prompt covering multiple files.

    Each element is (file_path, file_content, signals, truncated).
    """
    sections = [
        (
            f"===FILE: {file_path}===\n"
            f"{_truncation_note(truncated)}\n"
            f"Detected signal lines to classify:\n{_build_signal_summary(signals, signal_to_ast)}\n\n"
            f"File content:\n```\n{file_content}\n```"
        )
        for file_path, file_content, signals, truncated in files
    ]
    return (
        "\n\n".join(sections)
        + "\n\nClassify ALL signal lines across ALL files and return the JSON object."
    )


def parse_classification_response(
    data: dict,
) -> dict[int, tuple[SignalValidity, SignalDirection, float, str]]:
    """Parse a classification JSON response into a {line: (validity, direction, confidence, reason)} map.

    Expects a dict with an ``"integrations"`` key containing a list of entries,
    each with ``line``, ``label``, ``confidence``, and ``reason`` fields.
    """
    entries = data.get("integrations", [])
    logger.debug("Parsed %d integration entries from LLM response", len(entries))

    result: dict[int, tuple[SignalValidity, SignalDirection, float, str]] = {}
    for entry in entries:
        line = entry.get("line")
        label_str = entry.get("label", "NOT_INTEGRATION")
        confidence = float(entry.get("confidence", 0.5))
        reason = entry.get("reason", "")

        validity, direction = map_label_to_validity_direction(label_str)

        if line is not None:
            result[int(line)] = (validity, direction, confidence, reason)
            logger.debug(
                "  Line %4d  %-20s  %-10s  conf=%.2f  %s",
                int(line),
                validity.value,
                direction.value,
                confidence,
                reason[:60],
            )

    return result


def parse_batched_classification_response(
    data: dict,
) -> dict[str, dict[int, tuple[SignalValidity, SignalDirection, float, str]]]:
    """Parse a batched classification JSON response.

    Returns ``{file_path: {line: (validity, direction, confidence, reason)}}``.
    """
    entries = data.get("integrations", [])
    logger.debug(
        "Parsed %d integration entries from batched LLM response", len(entries)
    )

    result: dict[str, dict[int, tuple[SignalValidity, SignalDirection, float, str]]] = (
        defaultdict(dict)
    )
    for entry in entries:
        file_path = entry.get("file", "")
        line = entry.get("line")
        label_str = entry.get("label", "NOT_INTEGRATION")
        confidence = float(entry.get("confidence", 0.5))
        reason = entry.get("reason", "")

        validity, direction = map_label_to_validity_direction(label_str)

        if line is not None and file_path:
            result[file_path][int(line)] = (validity, direction, confidence, reason)

    return dict(result)


def deduplicate_signals(
    signals: list[IntegrationSignal],
) -> list[CompositeIntegrationSignal]:
    """Merge duplicate signals sharing (file_path, line_number, line_content).

    Groups all signals with the same source line into a single
    CompositeIntegrationSignal, preserving all original match metadata.
    Single-match lines are wrapped in a 1-element composite.

    Args:
        signals: Raw integration signals, potentially with duplicates.

    Returns:
        Deduplicated list of CompositeIntegrationSignals, one per unique line.
    """
    groups: dict[tuple[str, int, str], list[IntegrationSignal]] = defaultdict(list)
    for sig in signals:
        key = (
            sig.match.file_path,
            sig.match.line_number,
            sig.match.line_content.strip(),
        )
        groups[key].append(sig)

    original_count = len(signals)
    composites = [
        CompositeIntegrationSignal(signals=tuple(group)) for group in groups.values()
    ]
    logger.info(
        "Deduplication: %d signals → %d unique lines",
        original_count,
        len(composites),
    )
    return composites
