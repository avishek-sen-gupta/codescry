"""Walk up the tree-sitter AST to find the enclosing structural node for a line."""

from tree_sitter import Node
from tree_sitter_language_pack import get_parser

from repo_surveyor.integration_patterns import Language
from repo_surveyor.detection.syntax_zone import LANGUAGE_TO_TS_NAME
from repo_surveyor.integration_concretiser.types import ASTContext


class _DefinitionNodes:
    """Tree-sitter node types that represent definitions/declarations."""

    SUFFIXES = ("_definition", "_declaration")

    EXACT = frozenset(
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


class _StatementNodes:
    """Tree-sitter node types that represent statement-level constructs."""

    SUFFIXES = ("_statement",)

    EXACT = frozenset(
        {
            "expression_statement",
            "assignment",
            "annotation",
        }
    )


class _LanguageStatementNodes:
    """Per-language statement-equivalent node types that do NOT follow
    the _statement/_definition/_declaration suffix convention.
    Supplements the suffix heuristic. Derived from cfg_roles.json."""

    MAPPING: dict[Language, frozenset[str]] = {
        Language.JAVA: frozenset({"annotation"}),
        Language.PYTHON: frozenset({"assignment"}),
        Language.TYPESCRIPT: frozenset(),
        Language.JAVASCRIPT: frozenset(),
        Language.GO: frozenset(),
        Language.RUST: frozenset(
            {
                "if_expression",
                "match_expression",
                "for_expression",
                "while_expression",
                "loop_expression",
                "break_expression",
                "continue_expression",
                "return_expression",
                "yield_expression",
                "try_expression",
                "try_block",
            }
        ),
        Language.KOTLIN: frozenset(
            {
                "if_expression",
                "when_expression",
                "try_expression",
                "jump_expression",
                "constructor_invocation",
                "constructor_delegation_call",
            }
        ),
        Language.SCALA: frozenset(
            {
                "if_expression",
                "match_expression",
                "for_expression",
                "while_expression",
                "do_while_expression",
                "return_expression",
                "try_expression",
            }
        ),
        Language.RUBY: frozenset(
            {
                "if",
                "elsif",
                "unless",
                "if_modifier",
                "unless_modifier",
                "conditional",
                "case",
                "case_match",
                "for",
                "while",
                "until",
                "while_modifier",
                "until_modifier",
                "break",
                "next",
                "return",
                "begin",
                "rescue",
                "rescue_modifier",
                "ensure",
                "call",
                "assignment",
            }
        ),
        Language.CSHARP: frozenset(
            {
                "invocation_expression",
                "throw_expression",
            }
        ),
        Language.PHP: frozenset(
            {
                "function_call_expression",
                "member_call_expression",
                "scoped_call_expression",
                "nullsafe_member_call_expression",
                "throw_expression",
                "match_expression",
            }
        ),
        Language.C: frozenset(),
        Language.CPP: frozenset(),
        Language.COBOL: frozenset(
            {
                "if_header",
                "else_header",
                "else_if_header",
                "evaluate_header",
                "perform_procedure",
                "on_exception_clause",
                "at_end",
                "eop",
            }
        ),
        Language.PASCAL: frozenset(),
    }


class _InvocationNodes:
    """Tree-sitter node types that represent method/function calls."""

    TYPES = frozenset(
        {
            "method_invocation",
            "call_expression",
            "call",
            "invocation_expression",
            "method_call",
            "method_call_expression",
            "function_call",
        }
    )


class _BoundaryNodes:
    """Tree-sitter node types that represent scope boundaries for walk-up.

    Walking past these nodes would escape into a parent scope, losing
    the tight context around the signal.
    """

    TYPES = frozenset(
        {
            "lambda_expression",
            "arrow_function",
            "anonymous_function",
            "closure_expression",
        }
    )


class _RootNodes:
    """Tree-sitter node types that represent root/module-level constructs."""

    TYPES = frozenset(
        {
            "module",
            "program",
            "translation_unit",
            "source_file",
            "compilation_unit",
        }
    )


FALLBACK_AST_CONTEXT = ASTContext(
    node_type="unknown",
    node_text="",
    start_line=0,
    end_line=0,
)


def _is_definition_node(node: Node) -> bool:
    """Return True if the node is a definition or declaration node."""
    node_type = node.type
    if node_type in _DefinitionNodes.EXACT:
        return True
    return any(node_type.endswith(suffix) for suffix in _DefinitionNodes.SUFFIXES)


def _is_statement_node(
    node: Node, language_extras: frozenset[str] = frozenset()
) -> bool:
    """Return True if the node is a statement-level node."""
    node_type = node.type
    if node_type in language_extras:
        return True
    if node_type in _StatementNodes.EXACT:
        return True
    return any(node_type.endswith(suffix) for suffix in _StatementNodes.SUFFIXES)


def _is_invocation_node(node: Node) -> bool:
    """Return True if the node is a method/function call node."""
    return node.type in _InvocationNodes.TYPES


def _is_boundary_node(node: Node) -> bool:
    """Return True if the node is a scope boundary (lambda, arrow fn, etc.)."""
    return node.type in _BoundaryNodes.TYPES


def _is_root_node(node: Node) -> bool:
    """Return True if the node is a root/module-level node."""
    return node.type in _RootNodes.TYPES


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


def _walk_up_to_structural(
    node: Node, language_extras: frozenset[str] = frozenset()
) -> Node:
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
        if best_statement is None and _is_statement_node(current, language_extras):
            best_statement = current
        current = current.parent

    if best_statement is not None:
        return best_statement
    return node


def _walk_up_to_statement(
    node: Node, language_extras: frozenset[str] = frozenset()
) -> Node:
    """Walk up the tree from node to the nearest statement or definition.

    Unlike ``_walk_up_to_structural`` which prefers definitions over
    statements, this returns the **first** structural ancestor encountered
    (whichever is narrowest), stopping before root nodes.
    """
    current = node
    while current is not None and not _is_root_node(current):
        if _is_statement_node(current, language_extras) or _is_definition_node(current):
            return current
        current = current.parent
    return node


def _walk_up_to_invocation(
    node: Node, language_extras: frozenset[str] = frozenset()
) -> Node:
    """Walk up the tree from node to the nearest method/function invocation.

    Stops at the **first** invocation-like ancestor (e.g. ``method_invocation``,
    ``call_expression``).  Also treats scope boundaries (lambda, arrow
    function, block) as stop points to avoid escaping into a parent scope
    that contains a much larger chained expression.

    If no invocation is found before hitting a boundary, statement,
    definition, or root node, falls back to whatever boundary or
    statement was encountered.
    """
    current = node
    while current is not None and not _is_root_node(current):
        if _is_invocation_node(current):
            return current
        if _is_statement_node(current, language_extras) or _is_definition_node(current):
            return current
        if _is_boundary_node(current):
            return current
        current = current.parent
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
        return FALLBACK_AST_CONTEXT

    parser = get_parser(ts_name)
    tree = parser.parse(file_content)
    line_0indexed = line_number - 1

    deepest = _deepest_named_node_at_line(tree.root_node, line_0indexed)
    if deepest is None:
        return FALLBACK_AST_CONTEXT

    language_extras = _LanguageStatementNodes.MAPPING.get(language, frozenset())
    structural = _walk_up_to_structural(deepest, language_extras)

    if _is_root_node(structural):
        return FALLBACK_AST_CONTEXT

    return ASTContext(
        node_type=structural.type,
        node_text=_node_text(structural, file_content),
        start_line=structural.start_point.row + 1,
        end_line=structural.end_point.row + 1,
    )


def extract_statement_context(
    file_content: bytes,
    language: Language,
    line_number: int,
) -> ASTContext:
    """Extract the narrowest enclosing statement-level AST context for a line.

    Similar to ``extract_ast_context`` but walks up to the **nearest**
    statement or definition node rather than preferring definitions.
    This yields tighter context suitable for embedding-based classification.

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
        return FALLBACK_AST_CONTEXT

    parser = get_parser(ts_name)
    tree = parser.parse(file_content)
    line_0indexed = line_number - 1

    deepest = _deepest_named_node_at_line(tree.root_node, line_0indexed)
    if deepest is None:
        return FALLBACK_AST_CONTEXT

    language_extras = _LanguageStatementNodes.MAPPING.get(language, frozenset())
    statement = _walk_up_to_statement(deepest, language_extras)

    if _is_root_node(statement):
        return FALLBACK_AST_CONTEXT

    return ASTContext(
        node_type=statement.type,
        node_text=_node_text(statement, file_content),
        start_line=statement.start_point.row + 1,
        end_line=statement.end_point.row + 1,
    )


def extract_invocation_context(
    file_content: bytes,
    language: Language,
    line_number: int,
) -> ASTContext:
    """Extract the nearest enclosing invocation-level AST context for a line.

    Walks up to the **first** method/function call ancestor (e.g.
    ``method_invocation``, ``call_expression``).  This captures just the
    call and its arguments — tighter than statement-level context, which
    for chained builder patterns can span an entire fluent expression.

    Falls back to the nearest statement or definition if no invocation
    ancestor exists (e.g. for annotations or import lines).

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
        return FALLBACK_AST_CONTEXT

    parser = get_parser(ts_name)
    tree = parser.parse(file_content)
    line_0indexed = line_number - 1

    deepest = _deepest_named_node_at_line(tree.root_node, line_0indexed)
    if deepest is None:
        return FALLBACK_AST_CONTEXT

    language_extras = _LanguageStatementNodes.MAPPING.get(language, frozenset())
    invocation = _walk_up_to_invocation(deepest, language_extras)

    if _is_root_node(invocation):
        return FALLBACK_AST_CONTEXT

    return ASTContext(
        node_type=invocation.type,
        node_text=_node_text(invocation, file_content),
        start_line=invocation.start_point.row + 1,
        end_line=invocation.end_point.row + 1,
    )
