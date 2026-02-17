"""Core data types for control flow graph construction.

Defines the graph primitives (nodes, edges, graph) and the Fragment
abstraction used by the recursive CFG builder.
"""

from dataclasses import dataclass
from enum import Enum

from repo_surveyor.cfg_constructor.types import ControlFlowRole


class EdgeKind(Enum):
    """Classification of control flow edges."""

    NORMAL = "normal"
    TRUE = "true"
    FALSE = "false"
    EXCEPTION = "exception"
    BACK = "back"
    FALLTHROUGH = "fallthrough"


@dataclass(frozen=True)
class CFGNode:
    """A single node in the control flow graph.

    Attributes:
        id: Monotonically increasing, unique per graph.
        node_type: Tree-sitter node type or synthetic type (``ENTRY``/``EXIT``).
        role: The control flow role that determined construction behaviour.
        start_point: ``(row, col)`` in the source file.
        end_point: ``(row, col)`` in the source file.
        text_snippet: First ~80 characters of source text, for debugging.
    """

    id: int
    node_type: str
    role: ControlFlowRole
    start_point: tuple[int, int]
    end_point: tuple[int, int]
    text_snippet: str


@dataclass(frozen=True)
class CFGEdge:
    """A directed edge between two CFG nodes."""

    source: int
    target: int
    kind: EdgeKind


@dataclass(frozen=True)
class CFGraph:
    """An immutable control flow graph.

    Attributes:
        nodes: All nodes in the graph.
        edges: All edges in the graph.
        entry: ID of the ``ENTRY`` sentinel node.
        exit: ID of the ``EXIT`` sentinel node.
    """

    nodes: tuple[CFGNode, ...]
    edges: tuple[CFGEdge, ...]
    entry: int
    exit: int


@dataclass(frozen=True)
class PendingEdge:
    """An unresolved outgoing edge that bubbles up to an enclosing scope.

    Attributes:
        source_id: The CFG node that originates the edge.
        label: Optional label for labeled break/continue (empty = unlabeled).
    """

    source_id: int
    label: str = ""


@dataclass(frozen=True)
class Fragment:
    """A mini-CFG produced by processing a subtree.

    Every subtree yields a Fragment with an entry point, normal exits,
    and pending edges (break, continue, throw, return) that bubble up
    to be resolved by enclosing scope handlers.

    Attributes:
        entry: Entry node ID, or ``-1`` for an empty fragment.
        exits: Normal exit node IDs.
        pending_breaks: Unresolved break edges (may carry labels).
        pending_continues: Unresolved continue edges (may carry labels).
        pending_throws: Unresolved throw node IDs.
        pending_returns: Unresolved return node IDs.
    """

    entry: int
    exits: frozenset[int]
    pending_breaks: tuple[PendingEdge, ...] = ()
    pending_continues: tuple[PendingEdge, ...] = ()
    pending_throws: tuple[int, ...] = ()
    pending_returns: tuple[int, ...] = ()


EMPTY_FRAGMENT = Fragment(entry=-1, exits=frozenset())
