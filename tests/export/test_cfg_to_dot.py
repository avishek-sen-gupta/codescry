"""Tests for cfg_to_dot module."""

from repo_surveyor.cfg_constructor.cfg_types import CFGEdge, CFGNode, CFGraph, EdgeKind
from repo_surveyor.cfg_constructor.types import ControlFlowRole
from repo_surveyor.export.cfg_to_dot import cfg_to_dot, filter_cfg_by_line_range


def _make_node(
    id: int,
    role: ControlFlowRole = ControlFlowRole.LEAF,
    node_type: str = "expression_statement",
    start_row: int = 5,
    end_row: int = 5,
    snippet: str = "x = 1",
) -> CFGNode:
    return CFGNode(
        id=id,
        node_type=node_type,
        role=role,
        start_point=(start_row, 0),
        end_point=(end_row, 10),
        text_snippet=snippet,
    )


def _make_sentinel(id: int, label: str) -> CFGNode:
    return CFGNode(
        id=id,
        node_type=label,
        role=ControlFlowRole.LEAF,
        start_point=(0, 0),
        end_point=(0, 0),
        text_snippet=label,
    )


def _minimal_cfg() -> CFGraph:
    """ENTRY -> one node -> EXIT."""
    entry = _make_sentinel(0, "ENTRY")
    body = _make_node(1)
    exit_node = _make_sentinel(2, "EXIT")
    return CFGraph(
        nodes=(entry, body, exit_node),
        edges=(
            CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL),
            CFGEdge(source=1, target=2, kind=EdgeKind.NORMAL),
        ),
        entry=0,
        exit=2,
    )


class TestCfgToDot:
    def test_produces_valid_digraph(self):
        dot = cfg_to_dot(_minimal_cfg())
        assert dot.startswith("digraph")
        assert "n0" in dot
        assert "n1" in dot
        assert "n2" in dot
        assert "n0 -> n1" in dot
        assert "n1 -> n2" in dot

    def test_sentinel_nodes_get_doublecircle(self):
        dot = cfg_to_dot(_minimal_cfg())
        assert "n0 [shape=doublecircle, fillcolor=" in dot
        assert "n2 [shape=doublecircle, fillcolor=" in dot

    def test_branch_node_gets_diamond(self):
        entry = _make_sentinel(0, "ENTRY")
        branch = _make_node(
            1, role=ControlFlowRole.BRANCH, node_type="if_statement", snippet="if x > 0"
        )
        exit_node = _make_sentinel(2, "EXIT")
        cfg = CFGraph(
            nodes=(entry, branch, exit_node),
            edges=(
                CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL),
                CFGEdge(source=1, target=2, kind=EdgeKind.TRUE),
            ),
            entry=0,
            exit=2,
        )
        dot = cfg_to_dot(cfg)
        assert "n1 [shape=diamond" in dot

    def test_loop_node_gets_hexagon(self):
        entry = _make_sentinel(0, "ENTRY")
        loop = _make_node(
            1,
            role=ControlFlowRole.LOOP,
            node_type="for_statement",
            snippet="for i in range(10)",
        )
        exit_node = _make_sentinel(2, "EXIT")
        cfg = CFGraph(
            nodes=(entry, loop, exit_node),
            edges=(CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL),),
            entry=0,
            exit=2,
        )
        dot = cfg_to_dot(cfg)
        assert "n1 [shape=hexagon" in dot

    def test_edge_colors_match_edgekind(self):
        entry = _make_sentinel(0, "ENTRY")
        branch = _make_node(1, role=ControlFlowRole.BRANCH)
        then = _make_node(2, start_row=6)
        else_ = _make_node(3, start_row=8)
        exit_node = _make_sentinel(4, "EXIT")
        cfg = CFGraph(
            nodes=(entry, branch, then, else_, exit_node),
            edges=(
                CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL),
                CFGEdge(source=1, target=2, kind=EdgeKind.TRUE),
                CFGEdge(source=1, target=3, kind=EdgeKind.FALSE),
                CFGEdge(source=2, target=4, kind=EdgeKind.NORMAL),
                CFGEdge(source=3, target=4, kind=EdgeKind.EXCEPTION),
            ),
            entry=0,
            exit=4,
        )
        dot = cfg_to_dot(cfg)
        assert 'color="#059669"' in dot
        assert 'color="#dc2626"' in dot
        # Exception edge has dotted style
        assert 'style="dotted"' in dot

    def test_back_edge_style(self):
        entry = _make_sentinel(0, "ENTRY")
        body = _make_node(1)
        exit_node = _make_sentinel(2, "EXIT")
        cfg = CFGraph(
            nodes=(entry, body, exit_node),
            edges=(
                CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL),
                CFGEdge(source=1, target=1, kind=EdgeKind.BACK),
                CFGEdge(source=1, target=2, kind=EdgeKind.NORMAL),
            ),
            entry=0,
            exit=2,
        )
        dot = cfg_to_dot(cfg)
        assert 'color="#7c3aed"' in dot
        assert 'style="dashed"' in dot

    def test_custom_title(self):
        dot = cfg_to_dot(_minimal_cfg(), title="My Function")
        assert 'digraph "My Function"' in dot
        assert 'label="My Function"' in dot


class TestFilterCfgByLineRange:
    def test_keeps_only_nodes_in_range(self):
        entry = _make_sentinel(0, "ENTRY")
        a = _make_node(1, start_row=2, end_row=2)
        b = _make_node(2, start_row=10, end_row=10)
        c = _make_node(3, start_row=20, end_row=20)
        exit_node = _make_sentinel(4, "EXIT")
        cfg = CFGraph(
            nodes=(entry, a, b, c, exit_node),
            edges=(
                CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL),
                CFGEdge(source=1, target=2, kind=EdgeKind.NORMAL),
                CFGEdge(source=2, target=3, kind=EdgeKind.NORMAL),
                CFGEdge(source=3, target=4, kind=EdgeKind.NORMAL),
            ),
            entry=0,
            exit=4,
        )
        filtered = filter_cfg_by_line_range(cfg, start=5, end=15)
        node_ids = {n.id for n in filtered.nodes}
        # Sentinels always kept, plus node b (row 10)
        assert node_ids == {0, 2, 4}
        # Only edge 2->... that has both endpoints in kept set; none qualify
        # since 1 and 3 are removed. Only sentinel-to-sentinel edges if any.
        assert all(
            e.source in node_ids and e.target in node_ids for e in filtered.edges
        )

    def test_sentinels_always_retained(self):
        entry = _make_sentinel(0, "ENTRY")
        exit_node = _make_sentinel(1, "EXIT")
        cfg = CFGraph(
            nodes=(entry, exit_node),
            edges=(),
            entry=0,
            exit=1,
        )
        filtered = filter_cfg_by_line_range(cfg, start=100, end=200)
        assert len(filtered.nodes) == 2
