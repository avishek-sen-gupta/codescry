"""Tests for CFG graph types: frozen checks, equality, EMPTY_FRAGMENT."""

import pytest

from repo_surveyor.cfg_constructor.cfg_types import (
    CFGEdge,
    CFGNode,
    CFGraph,
    EMPTY_FRAGMENT,
    EdgeKind,
    Fragment,
    PendingEdge,
)
from repo_surveyor.cfg_constructor.types import ControlFlowRole


class TestCFGNode:
    """Tests for CFGNode frozen dataclass."""

    def test_is_frozen(self):
        node = CFGNode(
            id=0,
            node_type="if_statement",
            role=ControlFlowRole.BRANCH,
            start_point=(0, 0),
            end_point=(0, 10),
            text_snippet="if (x)",
        )
        with pytest.raises(AttributeError):
            node.id = 1

    def test_equality(self):
        args = dict(
            id=1,
            node_type="return_statement",
            role=ControlFlowRole.RETURN,
            start_point=(5, 4),
            end_point=(5, 15),
            text_snippet="return x;",
        )
        assert CFGNode(**args) == CFGNode(**args)

    def test_inequality_different_id(self):
        base = dict(
            node_type="leaf",
            role=ControlFlowRole.LEAF,
            start_point=(0, 0),
            end_point=(0, 5),
            text_snippet="x",
        )
        assert CFGNode(id=0, **base) != CFGNode(id=1, **base)


class TestCFGEdge:
    """Tests for CFGEdge frozen dataclass."""

    def test_is_frozen(self):
        edge = CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL)
        with pytest.raises(AttributeError):
            edge.source = 2

    def test_equality(self):
        assert CFGEdge(source=0, target=1, kind=EdgeKind.TRUE) == CFGEdge(
            source=0, target=1, kind=EdgeKind.TRUE
        )

    def test_different_kind_not_equal(self):
        assert CFGEdge(source=0, target=1, kind=EdgeKind.TRUE) != CFGEdge(
            source=0, target=1, kind=EdgeKind.FALSE
        )


class TestCFGraph:
    """Tests for CFGraph frozen dataclass."""

    def test_is_frozen(self):
        graph = CFGraph(nodes=(), edges=(), entry=0, exit=1)
        with pytest.raises(AttributeError):
            graph.entry = 5

    def test_stores_nodes_and_edges(self):
        node = CFGNode(
            id=0,
            node_type="ENTRY",
            role=ControlFlowRole.LEAF,
            start_point=(0, 0),
            end_point=(0, 0),
            text_snippet="",
        )
        edge = CFGEdge(source=0, target=1, kind=EdgeKind.NORMAL)
        graph = CFGraph(nodes=(node,), edges=(edge,), entry=0, exit=1)
        assert graph.nodes == (node,)
        assert graph.edges == (edge,)


class TestEdgeKind:
    """Tests for EdgeKind enum values."""

    def test_all_values_present(self):
        expected = {"normal", "true", "false", "exception", "back", "fallthrough"}
        actual = {kind.value for kind in EdgeKind}
        assert actual == expected


class TestPendingEdge:
    """Tests for PendingEdge frozen dataclass."""

    def test_default_label_is_empty(self):
        pe = PendingEdge(source_id=5)
        assert pe.label == ""

    def test_labeled_pending_edge(self):
        pe = PendingEdge(source_id=5, label="outer")
        assert pe.label == "outer"

    def test_is_frozen(self):
        pe = PendingEdge(source_id=5)
        with pytest.raises(AttributeError):
            pe.source_id = 6


class TestFragment:
    """Tests for Fragment frozen dataclass."""

    def test_default_pending_collections_are_empty(self):
        frag = Fragment(entry=0, exits=frozenset({1}))
        assert frag.pending_breaks == ()
        assert frag.pending_continues == ()
        assert frag.pending_throws == ()
        assert frag.pending_returns == ()

    def test_is_frozen(self):
        frag = Fragment(entry=0, exits=frozenset({1}))
        with pytest.raises(AttributeError):
            frag.entry = 2

    def test_with_pending_edges(self):
        frag = Fragment(
            entry=0,
            exits=frozenset({1}),
            pending_breaks=(PendingEdge(source_id=2),),
            pending_continues=(PendingEdge(source_id=3, label="loop"),),
            pending_throws=(4,),
            pending_returns=(5,),
        )
        assert len(frag.pending_breaks) == 1
        assert frag.pending_breaks[0].source_id == 2
        assert frag.pending_continues[0].label == "loop"
        assert frag.pending_throws == (4,)
        assert frag.pending_returns == (5,)


class TestEmptyFragment:
    """Tests for the EMPTY_FRAGMENT sentinel."""

    def test_entry_is_negative_one(self):
        assert EMPTY_FRAGMENT.entry == -1

    def test_exits_is_empty(self):
        assert EMPTY_FRAGMENT.exits == frozenset()

    def test_all_pending_empty(self):
        assert EMPTY_FRAGMENT.pending_breaks == ()
        assert EMPTY_FRAGMENT.pending_continues == ()
        assert EMPTY_FRAGMENT.pending_throws == ()
        assert EMPTY_FRAGMENT.pending_returns == ()

    def test_is_frozen(self):
        with pytest.raises(AttributeError):
            EMPTY_FRAGMENT.entry = 0
