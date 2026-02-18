"""Data types for integration signal concretisation."""

from dataclasses import dataclass
from enum import Enum

from ..integration_detector import IntegrationSignal


class IntegrationDirection(Enum):
    """Direction of an integration point."""

    INWARD = "inward"
    OUTWARD = "outward"
    UNKNOWN = "unknown"


@dataclass(frozen=True)
class ASTContext:
    """Enclosing AST node context for an integration signal.

    Attributes:
        node_type: Tree-sitter node type, e.g. "function_definition".
        node_text: Full text of the enclosing node.
        start_line: 1-indexed start line of the enclosing node.
        end_line: 1-indexed end line of the enclosing node.
    """

    node_type: str
    node_text: str
    start_line: int
    end_line: int


@dataclass(frozen=True)
class ConcretisedSignal:
    """An integration signal after LLM concretisation.

    Attributes:
        original_signal: The raw detected signal.
        ast_context: The enclosing AST node context.
        is_definite: Whether the signal is a definite integration point.
        direction: Inward, outward, or unknown (when is_definite is False).
        reasoning: LLM's explanation for the classification.
    """

    original_signal: IntegrationSignal
    ast_context: ASTContext
    is_definite: bool
    direction: IntegrationDirection
    reasoning: str


@dataclass(frozen=True)
class ConcretisationResult:
    """Aggregated result of concretising a batch of integration signals.

    Attributes:
        concretised: All concretised signals.
        signals_submitted: Total signals sent for concretisation.
        signals_definite: Count of signals classified as definite.
        signals_discarded: Count of signals classified as not definite.
    """

    concretised: tuple[ConcretisedSignal, ...]
    signals_submitted: int
    signals_definite: int
    signals_discarded: int
