"""Control flow role schema for language-independent CFG construction."""

from dataclasses import dataclass, field
from enum import Enum

from repo_surveyor.integration_patterns.types import Language

# A child reference is either a tree-sitter field name string or a positional
# index (Nth named child) for grammars that use anonymous children.
ChildRef = str | int


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


class SemanticSlot:
    """Named constants for the semantic child-slots used in field mappings."""

    CONDITION = "condition"
    CONSEQUENCE = "consequence"
    ALTERNATIVE = "alternative"
    VALUE = "value"
    BODY = "body"
    INITIALIZER = "initializer"
    UPDATE = "update"
    HANDLER = "handler"
    FINALIZER = "finalizer"


VALID_SLOTS: dict[ControlFlowRole, frozenset[str]] = {
    ControlFlowRole.BRANCH: frozenset(
        {SemanticSlot.CONDITION, SemanticSlot.CONSEQUENCE, SemanticSlot.ALTERNATIVE}
    ),
    ControlFlowRole.SWITCH: frozenset({SemanticSlot.VALUE, SemanticSlot.BODY}),
    ControlFlowRole.LOOP: frozenset(
        {
            SemanticSlot.BODY,
            SemanticSlot.CONDITION,
            SemanticSlot.INITIALIZER,
            SemanticSlot.UPDATE,
        }
    ),
    ControlFlowRole.LOOP_POST_CONDITION: frozenset(
        {SemanticSlot.BODY, SemanticSlot.CONDITION}
    ),
    ControlFlowRole.TRY: frozenset(
        {SemanticSlot.BODY, SemanticSlot.HANDLER, SemanticSlot.FINALIZER}
    ),
}


@dataclass(frozen=True)
class FieldMapping:
    """Maps semantic slots to tree-sitter child field names or positional indices."""

    slots: dict[str, ChildRef] = field(default_factory=dict)


@dataclass(frozen=True)
class NodeCFGSpec:
    """Pairs a control flow role with an optional field mapping for a single node type."""

    role: ControlFlowRole = ControlFlowRole.LEAF
    field_mapping: FieldMapping = field(default_factory=FieldMapping)


_NULL_NODE_SPEC = NodeCFGSpec(role=ControlFlowRole.LEAF)


@dataclass(frozen=True)
class LanguageCFGSpec:
    """Pairs a language with a mapping from tree-sitter node types to control flow specs.

    Node types absent from ``node_specs`` implicitly map to a null-object
    ``NodeCFGSpec`` with role ``ControlFlowRole.LEAF``.
    """

    language: Language
    node_specs: dict[str, NodeCFGSpec]

    def role_for(self, node_type: str) -> ControlFlowRole:
        """Return the control flow role for a tree-sitter node type.

        Returns ``ControlFlowRole.LEAF`` for unmapped types.
        """
        return self.node_specs.get(node_type, _NULL_NODE_SPEC).role

    def spec_for(self, node_type: str) -> NodeCFGSpec:
        """Return the full node spec for a tree-sitter node type.

        Returns a null-object ``NodeCFGSpec`` (LEAF, no field mapping) for unmapped types.
        """
        return self.node_specs.get(node_type, _NULL_NODE_SPEC)
