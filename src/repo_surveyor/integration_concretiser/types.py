"""Data types for integration signal AST context extraction."""

from dataclasses import dataclass

from repo_surveyor.training.types import TrainingLabel
from repo_surveyor.detection.integration_detector import IntegrationSignal


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
    """An integration signal with its ML-predicted direction label.

    Attributes:
        original_signal: The original integration signal.
        ast_context: The enclosing AST node context.
        label: ML-predicted label (DEFINITE_INWARD, DEFINITE_OUTWARD, NOT_DEFINITE).
    """

    original_signal: IntegrationSignal
    ast_context: ASTContext
    label: TrainingLabel

    @property
    def is_definite(self) -> bool:
        return self.label != TrainingLabel.NOT_DEFINITE


@dataclass(frozen=True)
class ConcretisationResult:
    """Result of running ML concretisation over a set of signal groups.

    Attributes:
        concretised: All concretised signals (definite and non-definite).
        signals_submitted: Total number of signals processed.
        signals_definite: Number of signals with a definite label.
        signals_discarded: Number of signals labelled NOT_DEFINITE.
    """

    concretised: tuple[ConcretisedSignal, ...]
    signals_submitted: int
    signals_definite: int
    signals_discarded: int
