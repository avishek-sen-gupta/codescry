"""Claude-based training example generator.

Generates labelled code snippets for the integration classifier by
prompting Claude with a target (language, integration_type, label)
triple and representative patterns from the existing pattern registry.
"""

import json
import logging
from dataclasses import dataclass

from ..ml_classifier.types import CompletionResult
from ..ml_classifier.model_protocol import LLMModel
from .types import TrainingExample, TrainingLabel
from .coverage import CoverageEntry

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


def _make_example_id(
    language: str,
    integration_type: str,
    label: str,
    framework: str,
    seq: int,
) -> str:
    """Generate a unique example ID."""
    lang_slug = (
        language.lower().replace("#", "sharp").replace("+", "p").replace("/", "")
    )
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

    raw_examples = json.loads(text)
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


def generate_all(
    model: LLMModel,
    entries: list[CoverageEntry],
    examples_per_triple: int = _Defaults.EXAMPLES_PER_TRIPLE,
) -> list[GenerationResult]:
    """Generate training examples for all coverage entries and all labels.

    Args:
        model: The LLM model to use for generation.
        entries: Coverage entries defining valid (language, type) pairs.
        examples_per_triple: Number of examples per (language, type, label) triple.

    Returns:
        List of GenerationResult, one per triple.
    """
    results: list[GenerationResult] = []
    total = len(entries) * len(TrainingLabel)

    for i, entry in enumerate(entries):
        for label in TrainingLabel:
            triple_index = i * len(TrainingLabel) + list(TrainingLabel).index(label) + 1
            logger.info("Triple %d/%d", triple_index, total)

            result = generate_examples_for_triple(
                model=model,
                entry=entry,
                label=label,
                count=examples_per_triple,
            )
            results.append(result)

    return results
