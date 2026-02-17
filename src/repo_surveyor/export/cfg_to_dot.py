"""Convert a CFGraph to Graphviz DOT format.

Pure functions — no I/O, no side effects.
"""

from repo_surveyor.cfg_constructor.cfg_types import CFGEdge, CFGNode, CFGraph, EdgeKind
from repo_surveyor.cfg_constructor.types import ControlFlowRole


class _NodeStyle:
    """Shape constants keyed by ControlFlowRole."""

    SENTINEL = "doublecircle"
    BRANCH = "diamond"
    SWITCH = "diamond"
    LOOP = "hexagon"
    LOOP_POST_CONDITION = "hexagon"
    TRY = "trapezium"
    DEFAULT = "box"

    _ROLE_MAP: dict[ControlFlowRole, str] = {
        ControlFlowRole.BRANCH: BRANCH,
        ControlFlowRole.SWITCH: SWITCH,
        ControlFlowRole.LOOP: LOOP,
        ControlFlowRole.LOOP_POST_CONDITION: LOOP_POST_CONDITION,
        ControlFlowRole.TRY: TRY,
    }

    @classmethod
    def shape_for(cls, node: CFGNode, is_sentinel: bool) -> str:
        if is_sentinel:
            return cls.SENTINEL
        return cls._ROLE_MAP.get(node.role, cls.DEFAULT)


class _EdgeStyle:
    """Color/style constants keyed by EdgeKind."""

    TRUE_COLOR = "green"
    FALSE_COLOR = "red"
    EXCEPTION_COLOR = "red"
    EXCEPTION_STYLE = "dotted"
    BACK_COLOR = "purple"
    BACK_STYLE = "dashed"
    DEFAULT_COLOR = "black"
    DEFAULT_STYLE = "solid"

    @classmethod
    def attrs_for(cls, kind: EdgeKind) -> dict[str, str]:
        if kind == EdgeKind.TRUE:
            return {"color": cls.TRUE_COLOR, "label": "T"}
        if kind == EdgeKind.FALSE:
            return {"color": cls.FALSE_COLOR, "label": "F"}
        if kind == EdgeKind.EXCEPTION:
            return {"color": cls.EXCEPTION_COLOR, "style": cls.EXCEPTION_STYLE}
        if kind == EdgeKind.BACK:
            return {"color": cls.BACK_COLOR, "style": cls.BACK_STYLE}
        return {}


def _escape_dot(text: str) -> str:
    """Escape text for DOT label strings."""
    return text.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")


def _node_label(node: CFGNode) -> str:
    """Build a human-readable label for a CFG node."""
    snippet = node.text_snippet[:60]
    return f"{node.node_type}\\n{_escape_dot(snippet)}"


def _format_attrs(attrs: dict[str, str]) -> str:
    """Format a dict of DOT attributes into a bracketed string."""
    if not attrs:
        return ""
    pairs = ", ".join(f'{k}="{v}"' for k, v in attrs.items())
    return f" [{pairs}]"


def cfg_to_dot(cfg: CFGraph, title: str = "CFG") -> str:
    """Convert a CFGraph to a Graphviz DOT string.

    Args:
        cfg: The control flow graph to render.
        title: Graph title used as the label.

    Returns:
        A complete DOT ``digraph`` string.
    """
    sentinel_ids = frozenset({cfg.entry, cfg.exit})
    node_map = {node.id: node for node in cfg.nodes}

    lines = [
        f'digraph "{_escape_dot(title)}" {{',
        "  rankdir=TB;",
        f'  label="{_escape_dot(title)}";',
        "",
    ]

    for node in cfg.nodes:
        is_sentinel = node.id in sentinel_ids
        shape = _NodeStyle.shape_for(node, is_sentinel)
        label = _node_label(node)
        lines.append(f'  n{node.id} [shape={shape}, label="{label}"];')

    lines.append("")

    for edge in cfg.edges:
        attrs = _EdgeStyle.attrs_for(edge.kind)
        lines.append(f"  n{edge.source} -> n{edge.target}{_format_attrs(attrs)};")

    lines.append("}")
    return "\n".join(lines)


def filter_cfg_by_line_range(cfg: CFGraph, start: int, end: int) -> CFGraph:
    """Extract a sub-graph containing only nodes within a line range.

    Sentinel nodes (ENTRY/EXIT) are always retained.  Edges are kept
    only when both endpoints survive filtering.

    Args:
        cfg: The full control flow graph.
        start: Start line (inclusive, 0-based row).
        end: End line (inclusive, 0-based row).

    Returns:
        A new ``CFGraph`` scoped to the given line range.
    """
    sentinel_ids = frozenset({cfg.entry, cfg.exit})

    kept_nodes = tuple(
        node
        for node in cfg.nodes
        if node.id in sentinel_ids or (start <= node.start_point[0] <= end)
    )
    kept_ids = frozenset(node.id for node in kept_nodes)

    kept_edges = tuple(
        edge
        for edge in cfg.edges
        if edge.source in kept_ids and edge.target in kept_ids
    )

    return CFGraph(
        nodes=kept_nodes,
        edges=kept_edges,
        entry=cfg.entry,
        exit=cfg.exit,
    )
