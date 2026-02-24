"""Data model for evidence-based predicate verification of integration signals.

Defines the vocabulary of universal predicates (structural, file/path-based,
and textual) that can confirm or reject embedding-based signal classifications
without any LLM calls.
"""

from dataclasses import dataclass
from enum import Enum


class PredicateCategory(Enum):
    """Broad category of a predicate's evidence source."""

    STRUCTURAL = "structural"
    FILE_PATH = "file_path"
    TEXTUAL = "textual"


class PredicateName(Enum):
    """Universal predicate names for evidence verification.

    Structural (AST-based):
        IN_STRING_CONTEXT  - signal line is inside a string literal node
        IN_CONSTANT_DECL   - signal is in a constant/static-final declaration
        IN_ASSERTION        - enclosing node contains assert/expect patterns

    File/path-based:
        IN_TEST_FILE       - file path matches test file conventions
        IN_VENDOR_DIR      - file is under a vendor/third-party directory
        IN_CONFIG_DIR      - file is under a config/settings directory
        IN_GENERATED_FILE  - file appears to be auto-generated
        PATH_MATCHES       - generic regex match against file path

    Textual (line/block regex):
        IN_LOG_STATEMENT   - enclosing node contains logging calls
        ENCLOSING_FUNCTION_CALLS - regex match on enclosing node text
        SIBLING_LINE_MATCHES - regex match on lines within +/-5 of signal
        LINE_MATCHES        - regex match on the signal line itself
    """

    # Structural
    IN_STRING_CONTEXT = "IN_STRING_CONTEXT"
    IN_CONSTANT_DECL = "IN_CONSTANT_DECL"
    IN_ASSERTION = "IN_ASSERTION"

    # File/path
    IN_TEST_FILE = "IN_TEST_FILE"
    IN_VENDOR_DIR = "IN_VENDOR_DIR"
    IN_CONFIG_DIR = "IN_CONFIG_DIR"
    IN_GENERATED_FILE = "IN_GENERATED_FILE"
    PATH_MATCHES = "PATH_MATCHES"

    # Textual
    IN_LOG_STATEMENT = "IN_LOG_STATEMENT"
    ENCLOSING_FUNCTION_CALLS = "ENCLOSING_FUNCTION_CALLS"
    SIBLING_LINE_MATCHES = "SIBLING_LINE_MATCHES"
    LINE_MATCHES = "LINE_MATCHES"


PREDICATE_CATEGORIES: dict[PredicateName, PredicateCategory] = {
    PredicateName.IN_STRING_CONTEXT: PredicateCategory.STRUCTURAL,
    PredicateName.IN_CONSTANT_DECL: PredicateCategory.STRUCTURAL,
    PredicateName.IN_ASSERTION: PredicateCategory.STRUCTURAL,
    PredicateName.IN_TEST_FILE: PredicateCategory.FILE_PATH,
    PredicateName.IN_VENDOR_DIR: PredicateCategory.FILE_PATH,
    PredicateName.IN_CONFIG_DIR: PredicateCategory.FILE_PATH,
    PredicateName.IN_GENERATED_FILE: PredicateCategory.FILE_PATH,
    PredicateName.PATH_MATCHES: PredicateCategory.FILE_PATH,
    PredicateName.IN_LOG_STATEMENT: PredicateCategory.TEXTUAL,
    PredicateName.ENCLOSING_FUNCTION_CALLS: PredicateCategory.TEXTUAL,
    PredicateName.SIBLING_LINE_MATCHES: PredicateCategory.TEXTUAL,
    PredicateName.LINE_MATCHES: PredicateCategory.TEXTUAL,
}


@dataclass(frozen=True)
class ChecklistEntry:
    """A single predicate+weight check in a pattern's evidence checklist.

    Attributes:
        predicate: Which predicate to evaluate.
        weight: Score adjustment when predicate matches (negative = suppress).
        pattern_arg: Regex or extra argument for parametric predicates (empty
                     string for non-parametric ones).
    """

    predicate: PredicateName
    weight: float
    pattern_arg: str


@dataclass(frozen=True)
class PredicateResult:
    """Result of evaluating a single predicate against a signal.

    Attributes:
        predicate_name: Which predicate was evaluated.
        matched: Whether the predicate fired (True = condition holds).
        weight: The weight from the checklist entry (applied only if matched).
    """

    predicate_name: PredicateName
    matched: bool
    weight: float


@dataclass(frozen=True)
class EvidenceVerdict:
    """Aggregated result of evaluating all predicates for one signal.

    Attributes:
        predicate_results: Individual predicate outcomes.
        score_adjustment: Sum of weights for matched predicates.
        original_score: Raw embedding similarity score before adjustment.
        adjusted_score: Clamped score after applying the adjustment.
    """

    predicate_results: tuple[PredicateResult, ...]
    score_adjustment: float
    original_score: float
    adjusted_score: float
