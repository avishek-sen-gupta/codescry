"""Integration signal AST context grouping.

Groups raw integration signals by their enclosing AST node (function/method/class),
using tree-sitter walk-up to find the structural ancestor.
"""

from .grouper import SignalGroup, group_signals_by_ast_context
from .types import ASTContext

__all__ = [
    "ASTContext",
    "SignalGroup",
    "group_signals_by_ast_context",
]
