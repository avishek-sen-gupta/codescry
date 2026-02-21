"""Types for integration classifier training data."""

from dataclasses import dataclass, asdict
from enum import Enum


class TrainingLabel(Enum):
    """Classification labels for training examples."""

    DEFINITE_INWARD = "DEFINITE_INWARD"
    DEFINITE_OUTWARD = "DEFINITE_OUTWARD"
    NOT_DEFINITE = "NOT_DEFINITE"
    REJECTED = "REJECTED"


TRAINING_LABELS = (
    TrainingLabel.DEFINITE_INWARD,
    TrainingLabel.DEFINITE_OUTWARD,
    TrainingLabel.NOT_DEFINITE,
)


@dataclass(frozen=True)
class TrainingExample:
    """A single labelled training example for the integration classifier.

    Attributes:
        id: Unique identifier following the convention
            ``{language}__{integration_type}__{label}__{framework}__{seq}``.
        language: Programming language of the code snippet.
        integration_type: The integration type this example demonstrates.
        label: Classification label (DEFINITE_INWARD, DEFINITE_OUTWARD, NOT_DEFINITE).
        code_snippet: The source code fragment.
        signal_line_index: Zero-based index of the signal line within the snippet.
        signal_line_content: The exact text of the signal line.
        matched_pattern: The regex pattern string that matches the signal line.
        ast_node_type: The AST node type enclosing the signal (e.g. method_declaration).
        framework: The framework or library name (e.g. Spring, Express).
    """

    id: str
    language: str
    integration_type: str
    label: str
    code_snippet: str
    signal_line_index: int
    signal_line_content: str
    matched_pattern: str
    ast_node_type: str
    framework: str

    def to_dict(self) -> dict:
        """Serialise to a plain dictionary for JSONL export."""
        return asdict(self)
