"""Validation pipeline for generated training examples.

Two validation passes:
1. Schema validation — field types, bounds, signal line consistency.
2. Regex validation — signal line matches the claimed pattern and at least
   one pattern from the registered set for that (language, type) pair.
"""

import logging
import re
from dataclasses import dataclass
from enum import Enum

from repo_surveyor.integration_patterns import (
    get_patterns_for_language,
    LANGUAGE_MODULES,
)
from repo_surveyor.integration_patterns.types import IntegrationType, Language

from repo_surveyor.training.types import TrainingExample, TrainingLabel

logger = logging.getLogger(__name__)


class ValidationFailure(Enum):
    """Reasons a training example can fail validation."""

    MISSING_FIELD = "missing_field"
    INVALID_LABEL = "invalid_label"
    INVALID_LANGUAGE = "invalid_language"
    INVALID_INTEGRATION_TYPE = "invalid_integration_type"
    SIGNAL_INDEX_OUT_OF_BOUNDS = "signal_index_out_of_bounds"
    SIGNAL_CONTENT_MISMATCH = "signal_content_mismatch"
    MATCHED_PATTERN_NO_MATCH = "matched_pattern_no_match"
    NO_REGISTERED_PATTERN_MATCH = "no_registered_pattern_match"


@dataclass(frozen=True)
class ValidationResult:
    """Result of validating a single training example."""

    example: TrainingExample
    is_valid: bool
    failures: tuple[ValidationFailure, ...]
    failure_details: tuple[str, ...]


_VALID_LABELS = frozenset(label.value for label in TrainingLabel)
_VALID_LANGUAGES = frozenset(lang.value for lang in Language)
_VALID_INTEGRATION_TYPES = frozenset(itype.value for itype in IntegrationType)


def _validate_schema(example: TrainingExample) -> list[tuple[ValidationFailure, str]]:
    """Validate structural integrity of a training example."""
    failures: list[tuple[ValidationFailure, str]] = []

    required_fields = [
        ("id", example.id),
        ("language", example.language),
        ("integration_type", example.integration_type),
        ("label", example.label),
        ("code_snippet", example.code_snippet),
        ("signal_line_content", example.signal_line_content),
        ("matched_pattern", example.matched_pattern),
        ("ast_node_type", example.ast_node_type),
        ("framework", example.framework),
    ]

    for field_name, value in required_fields:
        if not value:
            failures.append(
                (ValidationFailure.MISSING_FIELD, f"Empty or missing: {field_name}")
            )

    if example.label not in _VALID_LABELS:
        failures.append(
            (
                ValidationFailure.INVALID_LABEL,
                f"Invalid label: {example.label!r}",
            )
        )

    if example.language not in _VALID_LANGUAGES:
        failures.append(
            (
                ValidationFailure.INVALID_LANGUAGE,
                f"Invalid language: {example.language!r}",
            )
        )

    if example.integration_type not in _VALID_INTEGRATION_TYPES:
        failures.append(
            (
                ValidationFailure.INVALID_INTEGRATION_TYPE,
                f"Invalid integration type: {example.integration_type!r}",
            )
        )

    lines = example.code_snippet.split("\n")
    if example.signal_line_index < 0 or example.signal_line_index >= len(lines):
        failures.append(
            (
                ValidationFailure.SIGNAL_INDEX_OUT_OF_BOUNDS,
                f"signal_line_index {example.signal_line_index} out of bounds "
                f"(snippet has {len(lines)} lines)",
            )
        )
    elif (
        lines[example.signal_line_index].strip() != example.signal_line_content.strip()
    ):
        failures.append(
            (
                ValidationFailure.SIGNAL_CONTENT_MISMATCH,
                f"Line at index {example.signal_line_index}: "
                f"{lines[example.signal_line_index].strip()!r} != "
                f"{example.signal_line_content.strip()!r}",
            )
        )

    return failures


def _validate_regex(example: TrainingExample) -> list[tuple[ValidationFailure, str]]:
    """Validate that the signal line matches the claimed and registered patterns."""
    failures: list[tuple[ValidationFailure, str]] = []

    try:
        if not re.search(example.matched_pattern, example.signal_line_content):
            failures.append(
                (
                    ValidationFailure.MATCHED_PATTERN_NO_MATCH,
                    f"Pattern {example.matched_pattern!r} does not match "
                    f"signal line {example.signal_line_content!r}",
                )
            )
    except re.error as e:
        failures.append(
            (
                ValidationFailure.MATCHED_PATTERN_NO_MATCH,
                f"Invalid regex {example.matched_pattern!r}: {e}",
            )
        )

    language = Language.from_name(example.language)
    if language is None:
        return failures

    try:
        itype = IntegrationType(example.integration_type)
    except ValueError:
        return failures

    lang_module = LANGUAGE_MODULES.get(language)
    all_frameworks = list(lang_module.FRAMEWORK_PATTERNS.keys()) if lang_module else []
    patterns_by_type = get_patterns_for_language(language, all_frameworks)
    registered_patterns = patterns_by_type.get(itype, [])

    has_match = any(
        _safe_regex_search(regex, example.signal_line_content)
        for regex, _confidence, _source, _direction in registered_patterns
    )

    if not has_match:
        failures.append(
            (
                ValidationFailure.NO_REGISTERED_PATTERN_MATCH,
                f"No registered pattern for {example.language}/{example.integration_type} "
                f"matches signal line {example.signal_line_content!r}",
            )
        )

    return failures


def _safe_regex_search(pattern: str, text: str) -> bool:
    """Attempt a regex search, returning False on invalid patterns."""
    try:
        return re.search(pattern, text) is not None
    except re.error:
        return False


def validate_example(example: TrainingExample) -> ValidationResult:
    """Run full validation on a single training example.

    Args:
        example: The training example to validate.

    Returns:
        ValidationResult with pass/fail status and any failure details.
    """
    schema_failures = _validate_schema(example)
    regex_failures = _validate_regex(example) if not schema_failures else []

    all_failures = schema_failures + regex_failures

    return ValidationResult(
        example=example,
        is_valid=len(all_failures) == 0,
        failures=tuple(f for f, _ in all_failures),
        failure_details=tuple(d for _, d in all_failures),
    )


@dataclass(frozen=True)
class BatchValidationResult:
    """Result of validating a batch of training examples."""

    total: int
    valid: int
    invalid: int
    valid_examples: tuple[TrainingExample, ...]
    invalid_results: tuple[ValidationResult, ...]


def validate_batch(examples: list[TrainingExample]) -> BatchValidationResult:
    """Validate a batch of training examples.

    Args:
        examples: List of examples to validate.

    Returns:
        BatchValidationResult with valid/invalid partitions.
    """
    results = [validate_example(e) for e in examples]
    valid = [r for r in results if r.is_valid]
    invalid = [r for r in results if not r.is_valid]

    for r in invalid:
        logger.warning(
            "Invalid example %s: %s",
            r.example.id,
            "; ".join(r.failure_details),
        )

    return BatchValidationResult(
        total=len(results),
        valid=len(valid),
        invalid=len(invalid),
        valid_examples=tuple(r.example for r in valid),
        invalid_results=tuple(invalid),
    )
