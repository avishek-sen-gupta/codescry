"""Data types for integration signal AST context extraction."""

from dataclasses import dataclass


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
