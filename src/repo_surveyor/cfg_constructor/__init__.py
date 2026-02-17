"""CFG constructor module for language-independent control flow graph construction."""

from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec, load_cfg_roles
from repo_surveyor.cfg_constructor.cfg_types import (
    CFGEdge,
    CFGNode,
    CFGraph,
    EdgeKind,
    EMPTY_FRAGMENT,
    Fragment,
    PendingEdge,
)
from repo_surveyor.cfg_constructor.types import (
    ControlFlowRole,
    FieldMapping,
    LanguageCFGSpec,
    NodeCFGSpec,
    SemanticSlot,
)

__all__ = [
    "CFGEdge",
    "CFGNode",
    "CFGraph",
    "ControlFlowRole",
    "EMPTY_FRAGMENT",
    "EdgeKind",
    "FieldMapping",
    "Fragment",
    "LanguageCFGSpec",
    "NodeCFGSpec",
    "PendingEdge",
    "SemanticSlot",
    "get_cfg_spec",
    "load_cfg_roles",
]
