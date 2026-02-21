"""Claude-based training example generator.

Generates labelled code snippets for the integration classifier by
prompting Claude with a target (language, integration_type, label)
triple and representative patterns from the existing pattern registry.
"""

from __future__ import annotations

import json
import logging
import re
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

from repo_surveyor.ml_classifier.types import CompletionResult
from repo_surveyor.ml_classifier.model_protocol import LLMModel
from repo_surveyor.training.types import TRAINING_LABELS, TrainingExample, TrainingLabel
from repo_surveyor.training.coverage import CoverageEntry

if TYPE_CHECKING:
    from repo_surveyor.ml_classifier.claude_model import ClaudeClassifierModel

logger = logging.getLogger(__name__)


class _Defaults:
    """Default configuration for training data generation."""

    EXAMPLES_PER_TRIPLE = 5
    TEMPERATURE = 0.8


_SYSTEM_PROMPT = """\
You are a training data generator for a code integration classifier.

You generate realistic code snippets that demonstrate integration patterns
in source code. Each snippet must be syntactically valid code in the
specified language.

Classification rules (what makes a signal DEFINITE vs NOT_DEFINITE):
- Import statements are NOT definite integration points.
- Type annotations and interface declarations alone are NOT definite.
- Configuration declarations (e.g., connection strings, URLs in config) are NOT definite.
- Annotations on methods (e.g., @GetMapping, @PostMapping) ARE definite — they define active endpoints.
- Method calls to external systems (e.g., requests.get, db.execute, queue.publish) ARE definite.
- Framework handler registrations (e.g., app.get('/path', handler)) ARE definite.

Direction rules:
- INWARD: receiving — HTTP handler, message consumer, event listener, server endpoint.
- OUTWARD: sending — HTTP client call, message publish, database write/read, external API call.

Output ONLY a JSON array of objects. No markdown fencing, no explanation.
Each object must have exactly these fields:
- "code_snippet": string — the complete code snippet (multi-line, use \\n for newlines)
- "signal_line_index": integer — 0-based index of the signal line within the snippet
- "signal_line_content": string — the exact text of the signal line (must match the line at signal_line_index)
- "matched_pattern": string — the regex pattern that matches the signal line
- "ast_node_type": string — the AST node type (e.g., method_declaration, function_definition)
- "framework": string — the framework or library name (e.g., Spring, Express, stdlib)"""


_USER_PROMPT_TEMPLATE = """\
Generate {count} code snippets in {language} demonstrating {integration_type} \
integration signals classified as {label}.

The signal line in each snippet MUST match at least one of these regex patterns:
{patterns}

{label_guidance}

Requirements:
- Each snippet should be 3-15 lines of realistic, syntactically valid {language} code.
- Use diverse variable names, class names, and business logic across examples.
- The signal_line_content must exactly match the line at signal_line_index in code_snippet.
- The matched_pattern must be one of the patterns listed above that actually matches signal_line_content."""


_LABEL_GUIDANCE = {
    TrainingLabel.DEFINITE_INWARD: (
        "Each snippet must show an ACTIVE inward integration point — "
        "code that receives external requests or messages. "
        "Examples: HTTP endpoint handlers, message consumers, event listeners, "
        "server socket accept calls, gRPC service implementations."
    ),
    TrainingLabel.DEFINITE_OUTWARD: (
        "Each snippet must show an ACTIVE outward integration point — "
        "code that sends requests or data to external systems. "
        "Examples: HTTP client calls, database queries, message publishing, "
        "cache writes/reads, email sending, file uploads to remote storage."
    ),
    TrainingLabel.NOT_DEFINITE: (
        "Each snippet must show a NON-definite integration reference — "
        "code that mentions integration concepts but is NOT an active integration point. "
        "Vary across these subcategories:\n"
        "- Type declarations or interfaces (e.g., class UserRepository)\n"
        "- Configuration or bean setup (e.g., @Bean DataSource dataSource())\n"
        "- Annotations on classes rather than methods (e.g., @RestController on class)\n"
        "- Test or mock code (e.g., mockMvc.perform(get(...)))\n"
        "- Import statements that reference integration libraries"
    ),
}


def _format_patterns(patterns: tuple[str, ...]) -> str:
    """Format pattern regexes as a numbered list for the prompt."""
    return "\n".join(f"  {i + 1}. {p}" for i, p in enumerate(patterns))


def _build_user_prompt(
    language: str,
    integration_type: str,
    label: TrainingLabel,
    patterns: tuple[str, ...],
    count: int,
) -> str:
    """Build the user prompt for a generation request."""
    return _USER_PROMPT_TEMPLATE.format(
        count=count,
        language=language,
        integration_type=integration_type,
        label=label.value,
        patterns=_format_patterns(patterns),
        label_guidance=_LABEL_GUIDANCE[label],
    )


def _language_slug(language: str) -> str:
    """Slugify a language name to alphanumeric-and-underscore only."""
    return language.lower().replace("#", "sharp").replace("+", "p").replace("/", "")


def _make_example_id(
    language: str,
    integration_type: str,
    label: str,
    framework: str,
    seq: int,
) -> str:
    """Generate a unique example ID."""
    lang_slug = _language_slug(language)
    fw_slug = framework.lower().replace(" ", "_").replace("-", "_")
    return f"{lang_slug}__{integration_type}__{label.lower()}__{fw_slug}__{seq:03d}"


def _parse_generated_examples(
    response_text: str,
    language: str,
    integration_type: str,
    label: str,
) -> list[TrainingExample]:
    """Parse Claude's JSON array response into TrainingExample objects."""
    text = response_text.strip()
    if text.startswith("```"):
        first_newline = text.index("\n")
        last_fence = text.rfind("```")
        text = text[first_newline + 1 : last_fence].strip()

    try:
        raw_examples = json.loads(text)
    except json.JSONDecodeError:
        # Haiku sometimes emits invalid JSON escape sequences (e.g. \p, \s, \d
        # in code/regex snippets). Escape lone backslashes that are not part of
        # a valid JSON escape sequence and retry once.
        text = re.sub(r'\\(?!["\\/bfnrtu])', r"\\\\", text)
        try:
            raw_examples = json.loads(text)
        except json.JSONDecodeError as exc:
            logger.warning("Failed to parse JSON response: %s", exc)
            return []
    if not isinstance(raw_examples, list):
        logger.warning("Expected JSON array, got %s", type(raw_examples).__name__)
        return []

    examples: list[TrainingExample] = []
    for seq, raw in enumerate(raw_examples):
        framework = raw.get("framework", "unknown")
        example_id = _make_example_id(
            language, integration_type, label, framework, seq + 1
        )
        examples.append(
            TrainingExample(
                id=example_id,
                language=language,
                integration_type=integration_type,
                label=label,
                code_snippet=raw.get("code_snippet", ""),
                signal_line_index=raw.get("signal_line_index", 0),
                signal_line_content=raw.get("signal_line_content", ""),
                matched_pattern=raw.get("matched_pattern", ""),
                ast_node_type=raw.get("ast_node_type", ""),
                framework=framework,
            )
        )

    return examples


@dataclass(frozen=True)
class GenerationResult:
    """Result of generating training examples for one triple."""

    language: str
    integration_type: str
    label: str
    examples: tuple[TrainingExample, ...]
    prompt_tokens: int
    completion_tokens: int


def generate_examples_for_triple(
    model: LLMModel,
    entry: CoverageEntry,
    label: TrainingLabel,
    count: int = _Defaults.EXAMPLES_PER_TRIPLE,
) -> GenerationResult:
    """Generate training examples for a single (language, integration_type, label) triple.

    Args:
        model: The LLM model to use for generation.
        entry: Coverage entry with language, integration type, and sample patterns.
        label: The target classification label.
        count: Number of examples to generate.

    Returns:
        GenerationResult with parsed examples and token usage.
    """
    language = entry.language.value
    integration_type = entry.integration_type.value

    user_prompt = _build_user_prompt(
        language=language,
        integration_type=integration_type,
        label=label,
        patterns=entry.sample_patterns,
        count=count,
    )

    logger.info(
        "Generating %d examples: %s / %s / %s",
        count,
        language,
        integration_type,
        label.value,
    )

    result: CompletionResult = model.classify(_SYSTEM_PROMPT, user_prompt)

    examples = _parse_generated_examples(
        result.text, language, integration_type, label.value
    )

    logger.info(
        "Generated %d/%d examples (%d prompt tokens, %d completion tokens)",
        len(examples),
        count,
        result.prompt_tokens,
        result.completion_tokens,
    )

    return GenerationResult(
        language=language,
        integration_type=integration_type,
        label=label.value,
        examples=tuple(examples),
        prompt_tokens=result.prompt_tokens,
        completion_tokens=result.completion_tokens,
    )


def _triple_key(language: str, integration_type: str, label: str) -> str:
    """Build a unique key for a (language, integration_type, label) triple."""
    return f"{language}__{integration_type}__{label}"


def _batch_custom_id(language: str, integration_type: str, label: str) -> str:
    """Build a batch API-safe custom_id (^[a-zA-Z0-9_-]{1,64}$).

    Uses the same language slugification as _make_example_id so that
    special characters in language names (C++, C#, PL/I) are removed.
    """
    return f"{_language_slug(language)}__{integration_type}__{label}"


def _load_checkpoint(checkpoint_path: Path) -> list[GenerationResult]:
    """Load previously completed results from a checkpoint JSONL file."""
    if not checkpoint_path.exists():
        return []

    results: list[GenerationResult] = []
    with checkpoint_path.open("r", encoding="utf-8") as f:
        for line in f:
            raw = json.loads(line)
            examples = tuple(TrainingExample(**ex) for ex in raw["examples"])
            results.append(
                GenerationResult(
                    language=raw["language"],
                    integration_type=raw["integration_type"],
                    label=raw["label"],
                    examples=examples,
                    prompt_tokens=raw["prompt_tokens"],
                    completion_tokens=raw["completion_tokens"],
                )
            )

    logger.info("Loaded %d completed triples from checkpoint", len(results))
    return results


def _append_checkpoint(checkpoint_path: Path, result: GenerationResult) -> None:
    """Append a single GenerationResult to the checkpoint file."""
    record = {
        "language": result.language,
        "integration_type": result.integration_type,
        "label": result.label,
        "examples": [ex.to_dict() for ex in result.examples],
        "prompt_tokens": result.prompt_tokens,
        "completion_tokens": result.completion_tokens,
    }
    with checkpoint_path.open("a", encoding="utf-8") as f:
        f.write(json.dumps(record, ensure_ascii=False) + "\n")


def generate_all(
    model: LLMModel,
    entries: list[CoverageEntry],
    examples_per_triple: int = _Defaults.EXAMPLES_PER_TRIPLE,
    checkpoint_path: Path = Path(""),
    resume: bool = False,
) -> list[GenerationResult]:
    """Generate training examples for all coverage entries and all labels.

    Results are always checkpointed incrementally to ``checkpoint_path``
    (when non-empty) so that interrupted runs can be resumed.

    Args:
        model: The LLM model to use for generation.
        entries: Coverage entries defining valid (language, type) pairs.
        examples_per_triple: Number of examples per (language, type, label) triple.
        checkpoint_path: Path to checkpoint JSONL file.
            New results are appended after each triple completes.
        resume: When True, load completed triples from checkpoint and skip them.
            When False, start fresh (truncating any existing checkpoint).

    Returns:
        List of GenerationResult, one per triple.
    """
    has_checkpoint = str(checkpoint_path) != ""
    completed_results: list[GenerationResult] = []

    if has_checkpoint and resume:
        completed_results = _load_checkpoint(checkpoint_path)
    elif has_checkpoint:
        checkpoint_path.parent.mkdir(parents=True, exist_ok=True)
        checkpoint_path.write_text("")

    completed_keys = {
        _triple_key(r.language, r.integration_type, r.label) for r in completed_results
    }

    results: list[GenerationResult] = list(completed_results)
    total = len(entries) * len(TRAINING_LABELS)
    skipped = 0

    for i, entry in enumerate(entries):
        for label in TRAINING_LABELS:
            triple_index = (
                i * len(TRAINING_LABELS) + list(TRAINING_LABELS).index(label) + 1
            )
            key = _triple_key(
                entry.language.value, entry.integration_type.value, label.value
            )

            if key in completed_keys:
                skipped += 1
                logger.debug(
                    "Skipping completed triple %d/%d: %s", triple_index, total, key
                )
                continue

            logger.info("Triple %d/%d", triple_index, total)

            result = generate_examples_for_triple(
                model=model,
                entry=entry,
                label=label,
                count=examples_per_triple,
            )
            results.append(result)

            if has_checkpoint:
                _append_checkpoint(checkpoint_path, result)

    if skipped > 0:
        logger.info(
            "Skipped %d already-completed triples, generated %d new",
            skipped,
            len(results) - len(completed_results),
        )

    return results


def _build_batch_requests(
    entries: list[CoverageEntry],
    examples_per_triple: int,
) -> list[tuple[str, str, str]]:
    """Build all (custom_id, system_prompt, user_prompt) tuples for a batch.

    custom_id is slugified to satisfy the Batches API pattern ^[a-zA-Z0-9_-]{1,64}$.

    Returns:
        List of (custom_id, system_prompt, user_prompt) tuples.
    """
    return [
        (
            _batch_custom_id(
                entry.language.value, entry.integration_type.value, label.value
            ),
            _SYSTEM_PROMPT,
            _build_user_prompt(
                language=entry.language.value,
                integration_type=entry.integration_type.value,
                label=label,
                patterns=entry.sample_patterns,
                count=examples_per_triple,
            ),
        )
        for entry in entries
        for label in TRAINING_LABELS
    ]


def _save_batch_checkpoint(checkpoint_path: Path, batch_id: str) -> None:
    """Save batch ID to a checkpoint file for resume support."""
    checkpoint_path.parent.mkdir(parents=True, exist_ok=True)
    checkpoint_path.write_text(
        json.dumps({"batch_id": batch_id}, ensure_ascii=False), encoding="utf-8"
    )
    logger.info("Saved batch checkpoint: %s -> %s", batch_id, checkpoint_path)


def _load_batch_checkpoint(checkpoint_path: Path) -> str:
    """Load batch ID from a checkpoint file.

    Returns:
        The batch ID, or empty string if no checkpoint exists.
    """
    if not checkpoint_path.exists():
        return ""
    raw = json.loads(checkpoint_path.read_text(encoding="utf-8"))
    batch_id = raw.get("batch_id", "")
    if batch_id:
        logger.info("Loaded batch checkpoint: %s from %s", batch_id, checkpoint_path)
    return batch_id


def _parse_batch_results(
    batch_results: dict,
    entries: list[CoverageEntry],
) -> list[GenerationResult]:
    """Parse batch API results into GenerationResult objects.

    Args:
        batch_results: Dict mapping custom_id to BatchResult.
        entries: Coverage entries used to generate the batch.

    Returns:
        List of GenerationResult, one per successful triple.
    """
    results: list[GenerationResult] = []

    for entry in entries:
        for label in TRAINING_LABELS:
            key = _batch_custom_id(
                entry.language.value, entry.integration_type.value, label.value
            )
            batch_result = batch_results.get(key)

            if batch_result is None:
                logger.warning("Missing batch result for %s", key)
                continue

            if not batch_result.succeeded:
                logger.warning("Batch request failed for %s", key)
                continue

            examples = _parse_generated_examples(
                batch_result.completion.text,
                entry.language.value,
                entry.integration_type.value,
                label.value,
            )

            results.append(
                GenerationResult(
                    language=entry.language.value,
                    integration_type=entry.integration_type.value,
                    label=label.value,
                    examples=tuple(examples),
                    prompt_tokens=batch_result.completion.prompt_tokens,
                    completion_tokens=batch_result.completion.completion_tokens,
                )
            )

    return results


def generate_all_batch(
    model: ClaudeClassifierModel,
    entries: list[CoverageEntry],
    examples_per_triple: int = _Defaults.EXAMPLES_PER_TRIPLE,
    batch_checkpoint_path: Path = Path(""),
    resume: bool = False,
) -> list[GenerationResult]:
    """Generate training examples for all triples using the Batches API.

    Submits all requests as a single batch for asynchronous processing
    at 50% cost savings. Polls for completion, then parses results.

    Args:
        model: ClaudeClassifierModel with batch API access.
        entries: Coverage entries defining valid (language, type) pairs.
        examples_per_triple: Number of examples per triple.
        batch_checkpoint_path: Path to save/load batch ID for resume.
        resume: When True, attempt to resume from a saved batch ID.

    Returns:
        List of GenerationResult, one per triple.
    """
    has_checkpoint = batch_checkpoint_path != Path("")
    batch_id = ""

    if has_checkpoint and resume:
        batch_id = _load_batch_checkpoint(batch_checkpoint_path)

    if not batch_id:
        requests = _build_batch_requests(entries, examples_per_triple)
        logger.info("Submitting batch with %d requests", len(requests))
        batch_id = model.create_batch(requests, temperature=_Defaults.TEMPERATURE)
        if has_checkpoint:
            _save_batch_checkpoint(batch_checkpoint_path, batch_id)

    logger.info("Polling batch %s for completion...", batch_id)
    model.poll_batch(batch_id)

    logger.info("Retrieving batch results...")
    batch_results = model.retrieve_batch_results(batch_id)

    results = _parse_batch_results(batch_results, entries)
    logger.info(
        "Batch complete: %d/%d triples succeeded",
        len(results),
        len(entries) * len(TRAINING_LABELS),
    )

    return results
