"""Control flow role schema for language-independent CFG construction."""

from dataclasses import dataclass
from enum import Enum

from repo_surveyor.integration_patterns.types import Language


class ControlFlowRole(Enum):
    """Structural roles that map tree-sitter node types to CFG construction rules.

    Each role defines the control flow semantics for a class of syntax nodes.
    An LLM classifies tree-sitter node types into these roles per language,
    and the CFG builder dispatches on roles rather than node type names.
    """

    SEQUENCE = "sequence"
    BRANCH = "branch"
    SWITCH = "switch"
    LOOP = "loop"
    LOOP_POST_CONDITION = "loop_post_condition"
    RETURN = "return"
    BREAK = "break"
    CONTINUE = "continue"
    THROW = "throw"
    TRY = "try"
    CALL = "call"
    LEAF = "leaf"


@dataclass(frozen=True)
class LanguageCFGSpec:
    """Pairs a language with a mapping from tree-sitter node types to control flow roles.

    Node types absent from ``node_specs`` implicitly map to ``ControlFlowRole.LEAF``.
    """

    language: Language
    node_specs: dict[str, ControlFlowRole]

    def role_for(self, node_type: str) -> ControlFlowRole:
        """Return the control flow role for a tree-sitter node type.

        Returns ``ControlFlowRole.LEAF`` for unmapped types.
        """
        return self.node_specs.get(node_type, ControlFlowRole.LEAF)
