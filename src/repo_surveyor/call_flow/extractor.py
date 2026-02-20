"""Core logic for extracting a method call tree via LSP go-to-definition."""

from tree_sitter import Query, QueryCursor
from tree_sitter_language_pack import get_language, get_parser

from repo_surveyor.lsp_bridge import DocumentSymbol, LspBridgeClient

from repo_surveyor.call_flow.types import CallTree

# LSP SymbolKind: 6 = Method, 9 = Constructor, 12 = Function
_METHOD_KINDS = {6, 9, 12}

_CALL_QUERIES: dict[str, str] = {
    "java": "(method_invocation name: (identifier) @name)",
}


def _base_name(symbol_name: str) -> str:
    """Strip parameter signature: 'layout(String)' -> 'layout'."""
    paren = symbol_name.find("(")
    return symbol_name[:paren] if paren != -1 else symbol_name


def _collect_methods(
    symbols: list[DocumentSymbol], out: dict[str, list[dict[str, int]]]
) -> None:
    """Recursively collect method symbols from a DocumentSymbol tree."""
    for sym in symbols:
        if sym.kind in _METHOD_KINDS:
            name = _base_name(sym.name)
            out.setdefault(name, []).append(
                {
                    "start": sym.range_start_line,
                    "end": sym.range_end_line,
                }
            )
        for child in sym.children:
            _collect_methods([child], out)


def _build_method_map(
    symbols: list[DocumentSymbol],
) -> dict[str, list[dict[str, int]]]:
    """Build a map of method name -> list of {start, end} line ranges."""
    methods: dict[str, list[dict[str, int]]] = {}
    _collect_methods(symbols, methods)
    return methods


def _find_method_containing(
    method_map: dict[str, list[dict[str, int]]], line: int
) -> str:
    """Return the method name whose range contains the given line, or empty string."""
    for name, ranges in method_map.items():
        for r in ranges:
            if r["start"] <= line <= r["end"]:
                return name
    return ""


def _normalise_uri(uri: str) -> str:
    """Strip the file:// prefix for path comparison."""
    return uri.removeprefix("file://")


def _extract_call_identifiers(
    query: Query, tree, start_line: int, end_line: int
) -> list[tuple[int, int, str]]:
    """Find method invocations within a line range using tree-sitter.

    Returns a list of (line, character, identifier) tuples.
    """
    cursor = QueryCursor(query)
    cursor.set_point_range((start_line, 0), (end_line + 1, 0))
    captures = cursor.captures(tree.root_node)
    return [
        (node.start_point[0], node.start_point[1], node.text.decode())
        for node in captures.get("name", [])
    ]


def extract_call_tree(
    client: LspBridgeClient,
    file_uri: str,
    file_path: str,
    source_text: str,
    entry_method: str,
    language: str,
) -> CallTree:
    """Walk the call tree starting from *entry_method* using LSP go-to-definition.

    Args:
        client: An LSP bridge client used for symbol and definition requests.
        file_uri: The ``file://`` URI of the target file.
        file_path: The local filesystem path of the target file.
        source_text: The full source text of the file.
        entry_method: The name of the method to start tracing from.
        language: The language identifier (e.g. ``"java"``).

    Returns:
        A :class:`CallTree` containing the entry method and all discovered edges.

    Raises:
        ValueError: If *language* has no registered call query or
                     *entry_method* is not found among the document symbols.
    """
    if language not in _CALL_QUERIES:
        raise ValueError(
            f"No call query registered for language '{language}'. "
            f"Supported: {sorted(_CALL_QUERIES)}"
        )

    ts_language = get_language(language)
    parser = get_parser(language)
    query = Query(ts_language, _CALL_QUERIES[language])
    tree = parser.parse(source_text.encode("utf-8"))

    symbols = client.get_symbols(file_uri)
    method_map = _build_method_map(symbols)

    if entry_method not in method_map:
        raise ValueError(
            f"Entry method '{entry_method}' not found in symbols. "
            f"Available: {sorted(method_map)}"
        )

    edges: dict[str, set[str]] = {}
    visited: set[str] = set()

    def walk(method_name: str) -> None:
        if method_name in visited:
            return
        visited.add(method_name)

        callees: set[str] = set()
        for r in method_map[method_name]:
            candidates = _extract_call_identifiers(query, tree, r["start"], r["end"])
            for line, char, _ident in candidates:
                locations = client.get_definition(file_uri, line, char)
                for loc in locations:
                    target_uri = _normalise_uri(loc.uri)
                    target_line = loc.range_start_line
                    if target_uri != file_path:
                        continue
                    callee = _find_method_containing(method_map, target_line)
                    if callee and callee != method_name:
                        callees.add(callee)

        edges[method_name] = callees
        for callee in callees:
            walk(callee)

    walk(entry_method)

    frozen_edges = {caller: frozenset(callees) for caller, callees in edges.items()}
    return CallTree(entry_method=entry_method, edges=frozen_edges)


def format_call_tree(call_tree: CallTree) -> str:
    """Return an indented tree string representing the call tree."""
    lines: list[str] = []

    def _format(root: str, indent: int, visited: set[str]) -> None:
        prefix = "  " * indent + ("-> " if indent > 0 else "")
        suffix = " (recursive)" if root in visited else ""
        lines.append(f"{prefix}{root}(){suffix}")
        if root in visited:
            return
        visited.add(root)
        for callee in sorted(call_tree.edges.get(root, frozenset())):
            _format(callee, indent + 1, visited)

    _format(call_tree.entry_method, 0, set())
    return "\n".join(lines)
