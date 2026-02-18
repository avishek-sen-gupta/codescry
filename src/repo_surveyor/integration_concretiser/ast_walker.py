"""Walk up the tree-sitter AST to find the enclosing structural node for a line."""

from tree_sitter import Node
from tree_sitter_language_pack import get_parser

from ..integration_patterns import Language
from ..syntax_zone import LANGUAGE_TO_TS_NAME
from .types import ASTContext

_DEFINITION_SUFFIXES = ("_definition", "_declaration")

_DEFINITION_EXACT = frozenset(
    {
        "decorated_definition",
        "class_declaration",
        "method_declaration",
        "function_declaration",
        "function_definition",
        "class_definition",
        "lexical_declaration",
        "variable_declaration",
        "field_declaration",
        "property_declaration",
    }
)

_STATEMENT_SUFFIXES = ("_statement",)

_STATEMENT_EXACT = frozenset(
    {
        "expression_statement",
        "assignment",
        "annotation",
    }
)

_ROOT_NODE_TYPES = frozenset(
    {
        "module",
        "program",
        "translation_unit",
        "source_file",
        "compilation_unit",
    }
)

_FALLBACK_AST_CONTEXT = ASTContext(
    node_type="unknown",
    node_text="",
    start_line=0,
    end_line=0,
)


def _is_definition_node(node: Node) -> bool:
    """Return True if the node is a definition or declaration node."""
    node_type = node.type
    if node_type in _DEFINITION_EXACT:
        return True
    return any(node_type.endswith(suffix) for suffix in _DEFINITION_SUFFIXES)


def _is_statement_node(node: Node) -> bool:
    """Return True if the node is a statement-level node."""
    node_type = node.type
    if node_type in _STATEMENT_EXACT:
        return True
    return any(node_type.endswith(suffix) for suffix in _STATEMENT_SUFFIXES)


def _is_root_node(node: Node) -> bool:
    """Return True if the node is a root/module-level node."""
    return node.type in _ROOT_NODE_TYPES


def _deepest_named_node_at_line(root: Node, line_0indexed: int) -> Node | None:
    """Find the deepest named node whose range covers the given line."""
    best: Node | None = None
    stack: list[Node] = [root]

    while stack:
        node = stack.pop()
        if node.start_point.row > line_0indexed or node.end_point.row < line_0indexed:
            continue
        if node.is_named:
            best = node
        stack.extend(reversed(node.children))

    return best


def _walk_up_to_structural(node: Node) -> Node:
    """Walk up the tree from node to the enclosing definition/declaration.

    Strategy: first try to find a definition/declaration ancestor.
    If none exists before the root, fall back to the nearest statement ancestor.
    If neither exists, return the original node.
    """
    best_statement: Node | None = None
    current = node

    while current is not None and not _is_root_node(current):
        if _is_definition_node(current):
            return current
        if best_statement is None and _is_statement_node(current):
            best_statement = current
        current = current.parent

    if best_statement is not None:
        return best_statement
    return node


def _node_text(node: Node, file_content: bytes) -> str:
    """Extract the text of a node from the file content."""
    return file_content[node.start_byte : node.end_byte].decode(
        "utf-8", errors="replace"
    )


def extract_ast_context(
    file_content: bytes,
    language: Language,
    line_number: int,
) -> ASTContext:
    """Extract the enclosing AST node context for a given line.

    Parses the file with tree-sitter, finds the deepest named node at
    the given line, then walks up to the nearest definition/declaration
    node (preferred) or statement node (fallback).

    Args:
        file_content: Raw bytes of the source file.
        language: Programming language of the file.
        line_number: 1-indexed line number of the signal.

    Returns:
        ASTContext with the enclosing node's type, text, and line range.
        Returns a fallback context if the language is unsupported or
        the line cannot be resolved.
    """
    ts_name = LANGUAGE_TO_TS_NAME.get(language)
    if ts_name is None:
        return _FALLBACK_AST_CONTEXT

    parser = get_parser(ts_name)
    tree = parser.parse(file_content)
    line_0indexed = line_number - 1

    deepest = _deepest_named_node_at_line(tree.root_node, line_0indexed)
    if deepest is None:
        return _FALLBACK_AST_CONTEXT

    structural = _walk_up_to_structural(deepest)

    # If the structural node is the original deepest node and it's
    # a root-level node, return fallback (e.g., empty file parsed to module)
    if _is_root_node(structural):
        return _FALLBACK_AST_CONTEXT

    return ASTContext(
        node_type=structural.type,
        node_text=_node_text(structural, file_content),
        start_line=structural.start_point.row + 1,
        end_line=structural.end_point.row + 1,
    )
