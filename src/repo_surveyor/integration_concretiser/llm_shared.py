"""Shared utilities for LLM-based integration signal concretisers.

Contains the system prompt, prompt builder, response parser, and file reader
used by both the Ollama and Gemini concretisers.
"""

import json
import logging

from repo_surveyor.detection.integration_detector import IntegrationSignal
from repo_surveyor.training.types import TrainingLabel

logger = logging.getLogger(__name__)

SYSTEM_PROMPT = """You are an expert code analyst specialising in detecting integration boundaries in software systems.

Given a source file and a list of lines flagged as potential integration signals, classify each signal line.

Respond ONLY with a JSON object — no prose, no markdown fences — in this exact shape:
{
  "integrations": [
    {"line": <int>, "label": "<LABEL>", "confidence": <float 0-1>, "reason": "<one sentence>"},
    ...
  ]
}

Valid labels:
  DEFINITE_INWARD   — the code exposes a service boundary to callers:
                       HTTP/REST/gRPC server endpoint, message consumer/listener,
                       socket server accept, SSE emitter, scheduled job entry point.
  DEFINITE_OUTWARD  — the code calls an external system:
                       HTTP client call, database connection/query, file write,
                       message publish/send, cache store, SMTP send, FTP upload.
  NOT_DEFINITE      — the match is incidental and not a real integration boundary:
                       variable/field named 'file' or 'cache', string literal containing
                       a URL, test assertion, generated parser code, log statement,
                       import statement, or any other false positive.

Classify ALL signal lines listed. If a line number is not in the file or cannot be
determined, still include it with label NOT_DEFINITE."""

MAX_FILE_CHARS = 12_000

MAX_RETRIES = 5
INITIAL_BACKOFF_SECONDS = 2.0


def read_file_bytes(file_path: str) -> bytes:
    """Read a file and return its raw bytes."""
    with open(file_path, "rb") as f:
        return f.read()


def build_classification_prompt(
    file_path: str,
    file_content: str,
    signals: list[IntegrationSignal],
    truncated: bool,
) -> str:
    """Build the user prompt for LLM-based classification of a single file."""
    signal_lines = sorted(
        {
            (
                s.match.line_number,
                s.integration_type.value,
                s.match.line_content.strip(),
            )
            for s in signals
        }
    )
    signal_summary = "\n".join(
        f"  Line {ln}: [{itype}] {content}" for ln, itype, content in signal_lines
    )
    truncation_note = (
        f"\n[NOTE: File truncated to first {MAX_FILE_CHARS} chars; "
        "some signal lines may not appear in the excerpt shown below]"
        if truncated
        else ""
    )
    return (
        f"File: {file_path}{truncation_note}\n\n"
        f"Detected signal lines to classify:\n{signal_summary}\n\n"
        f"File content:\n```\n{file_content}\n```\n\n"
        "Classify each signal line and return the JSON object."
    )


def parse_classification_response(
    data: dict,
) -> dict[int, tuple[TrainingLabel, float, str]]:
    """Parse a classification JSON response into a {line: (label, confidence, reason)} map.

    Expects a dict with an ``"integrations"`` key containing a list of entries,
    each with ``line``, ``label``, ``confidence``, and ``reason`` fields.
    """
    entries = data.get("integrations", [])
    logger.debug("Parsed %d integration entries from LLM response", len(entries))

    result: dict[int, tuple[TrainingLabel, float, str]] = {}
    for entry in entries:
        line = entry.get("line")
        label_str = entry.get("label", "NOT_DEFINITE")
        confidence = float(entry.get("confidence", 0.5))
        reason = entry.get("reason", "")

        try:
            label = TrainingLabel(label_str)
        except ValueError:
            logger.warning(
                "Unknown label %r for line %s — defaulting to NOT_DEFINITE",
                label_str,
                line,
            )
            label = TrainingLabel.NOT_DEFINITE

        if line is not None:
            result[int(line)] = (label, confidence, reason)
            logger.debug(
                "  Line %4d  %-20s  conf=%.2f  %s",
                int(line),
                label.value,
                confidence,
                reason[:60],
            )

    return result
