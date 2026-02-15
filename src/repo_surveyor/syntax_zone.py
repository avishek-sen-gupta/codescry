"""Tree-sitter based syntax zone classification for source files.

Parses source files with tree-sitter and classifies each line as belonging
to a syntax zone (code, comment, string literal, or import). This enables
filtering out false-positive integration signals from comments and strings.
"""

from dataclasses import dataclass
from enum import Enum

from tree_sitter import Node

from .integration_patterns import Language


class SyntaxZone(Enum):
    """Classification of a source line's syntax zone."""

    CODE = "code"
    COMMENT = "comment"
    STRING_LITERAL = "string_literal"
    IMPORT = "import"


@dataclass(frozen=True)
class SyntaxRange:
    """A contiguous range of lines belonging to a single syntax zone.

    Attributes:
        start_row: 0-indexed start line (inclusive).
        end_row: 0-indexed end line (inclusive).
        zone: The syntax zone classification.
    """

    start_row: int
    end_row: int
    zone: SyntaxZone


@dataclass(frozen=True)
class SyntaxRangeMap:
    """Sorted collection of syntax ranges for a parsed file.

    An empty ranges tuple means every line classifies as CODE
    (null-object pattern for unsupported languages).
    """

    ranges: tuple[SyntaxRange, ...]


class _NodeTypes:
    """Tree-sitter node type sets for zone classification."""

    COMMENT = frozenset({"comment", "line_comment", "block_comment"})

    STRING = frozenset(
        {
            "string",
            "string_literal",
            "raw_string_literal",
            "interpreted_string_literal",
            "template_string",
            "heredoc_body",
            "verbatim_string_literal",
        }
    )

    IMPORT = frozenset(
        {
            "import_declaration",
            "import_statement",
            "import_from_statement",
            "use_declaration",
            "using_directive",
        }
    )


class _TreeSitterLanguages:
    """Mapping from Language enum to tree-sitter grammar names."""

    MAPPING: dict[Language, str] = {
        Language.JAVA: "java",
        Language.PYTHON: "python",
        Language.TYPESCRIPT: "typescript",
        Language.JAVASCRIPT: "javascript",
        Language.GO: "go",
        Language.RUST: "rust",
        Language.CSHARP: "csharp",
        Language.KOTLIN: "kotlin",
        Language.SCALA: "scala",
        Language.RUBY: "ruby",
        Language.PHP: "php",
        Language.C: "c",
        Language.CPP: "cpp",
        Language.COBOL: "cobol",
    }


_EMPTY_RANGE_MAP = SyntaxRangeMap(ranges=())

_ZONE_BY_NODE_TYPE: dict[str, SyntaxZone] = {
    **{t: SyntaxZone.COMMENT for t in _NodeTypes.COMMENT},
    **{t: SyntaxZone.STRING_LITERAL for t in _NodeTypes.STRING},
    **{t: SyntaxZone.IMPORT for t in _NodeTypes.IMPORT},
}


def _classify_node(node_type: str) -> SyntaxZone | None:
    """Return the zone for a node type, or None if it's not a zone node."""
    return _ZONE_BY_NODE_TYPE.get(node_type)


def _line_is_whitespace_before(source_lines: list[bytes], row: int, col: int) -> bool:
    """Check if all content before column ``col`` on ``row`` is whitespace."""
    if row >= len(source_lines):
        return True
    return source_lines[row][:col].strip() == b""


def _line_is_whitespace_after(source_lines: list[bytes], row: int, col: int) -> bool:
    """Check if all content after column ``col`` on ``row`` is whitespace."""
    if row >= len(source_lines):
        return True
    return source_lines[row][col:].strip() == b""


def _effective_range(
    node: Node,
    zone: SyntaxZone,
    source_lines: list[bytes],
) -> SyntaxRange | None:
    """Compute the effective row range for a zone node, excluding mixed lines.

    A line is included in the range only if the zone node is the sole
    non-whitespace content on that line.  For multi-line nodes, the start
    and end rows are trimmed if they share the line with code.

    Returns None if no full lines are covered (e.g. an inline string).
    """
    start_row = node.start_point.row
    end_row = node.end_point.row
    start_col = node.start_point.column
    end_col = node.end_point.column

    effective_start = start_row
    effective_end = end_row

    # Trim start row if it has non-whitespace content before the node
    if not _line_is_whitespace_before(source_lines, start_row, start_col):
        effective_start = start_row + 1

    # Trim end row if it has non-whitespace content after the node
    if not _line_is_whitespace_after(source_lines, end_row, end_col):
        effective_end = end_row - 1

    if effective_start > effective_end:
        return None

    return SyntaxRange(
        start_row=effective_start,
        end_row=effective_end,
        zone=zone,
    )


def build_syntax_range_map(
    root_node: Node,
    source_lines: list[bytes],
) -> SyntaxRangeMap:
    """Walk the tree-sitter AST and collect syntax zone ranges.

    Performs a single-pass walk, recording ranges for comment, string,
    and import nodes. Does not recurse into matched nodes (children
    are subsumed by the parent range). Lines where a zone node shares
    the line with code are excluded (conservative: classify as CODE).

    Args:
        root_node: Root node of a tree-sitter parse tree.
        source_lines: Source file split into lines (bytes).

    Returns:
        SyntaxRangeMap with ranges sorted by start_row.
    """
    ranges: list[SyntaxRange] = []
    stack: list[Node] = [root_node]

    while stack:
        node = stack.pop()
        zone = _classify_node(node.type)
        if zone is not None:
            effective = _effective_range(node, zone, source_lines)
            if effective is not None:
                ranges.append(effective)
            # Don't recurse into matched nodes
            continue
        # Add children in reverse order so leftmost is processed first
        stack.extend(reversed(node.children))

    ranges.sort(key=lambda r: r.start_row)
    return SyntaxRangeMap(ranges=tuple(ranges))


def classify_line(range_map: SyntaxRangeMap, line_number: int) -> SyntaxZone:
    """Classify a source line by its syntax zone.

    Args:
        range_map: Pre-built syntax range map for the file.
        line_number: 1-indexed line number.

    Returns:
        The SyntaxZone for the line. CODE if no range covers it.
    """
    target = line_number - 1  # convert to 0-indexed

    for r in range_map.ranges:
        if r.start_row > target:
            break  # early termination — ranges are sorted
        if r.start_row <= target <= r.end_row:
            return r.zone

    return SyntaxZone.CODE


def parse_file_zones(content: bytes, language: Language) -> SyntaxRangeMap:
    """Parse a file with tree-sitter and build its syntax range map.

    For languages without tree-sitter support (e.g. PL/I), returns an
    empty range map so all lines classify as CODE (null-object pattern).

    Args:
        content: Raw file content as bytes.
        language: The detected programming language.

    Returns:
        SyntaxRangeMap for the file.
    """
    ts_name = _TreeSitterLanguages.MAPPING.get(language)
    if ts_name is None:
        return _EMPTY_RANGE_MAP

    from tree_sitter_language_pack import get_parser

    parser = get_parser(ts_name)
    tree = parser.parse(content)
    source_lines = content.split(b"\n")
    return build_syntax_range_map(tree.root_node, source_lines)
