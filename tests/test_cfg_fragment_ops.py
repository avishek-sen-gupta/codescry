"""Tests for fragment_ops: pure data tests with integer node IDs."""

import pytest

from repo_surveyor.cfg_constructor.cfg_types import (
    CFGEdge,
    EMPTY_FRAGMENT,
    EdgeKind,
    Fragment,
    PendingEdge,
)
from repo_surveyor.cfg_constructor.fragment_ops import (
    chain,
    make_atomic,
    make_terminal_break,
    make_terminal_continue,
    make_terminal_return,
    make_terminal_throw,
    merge_exits,
    merge_pending,
    resolve_breaks,
    resolve_continues,
    wire,
    wire_ids,
    wire_pending,
)


class TestWire:
    """Tests for wire()."""

    def test_single_source_to_target(self):
        edges = wire(frozenset({1}), 2, EdgeKind.NORMAL)

        assert edges == (CFGEdge(source=1, target=2, kind=EdgeKind.NORMAL),)

    def test_multiple_sources(self):
        edges = wire(frozenset({1, 3}), 5, EdgeKind.TRUE)

        assert len(edges) == 2
        targets = {e.target for e in edges}
        sources = {e.source for e in edges}
        assert targets == {5}
        assert sources == {1, 3}

    def test_empty_sources_returns_empty(self):
        assert wire(frozenset(), 5, EdgeKind.NORMAL) == ()

    def test_negative_target_returns_empty(self):
        assert wire(frozenset({1}), -1, EdgeKind.NORMAL) == ()


class TestWirePending:
    """Tests for wire_pending()."""

    def test_wires_pending_edges(self):
        pending = (PendingEdge(source_id=1), PendingEdge(source_id=2))
        edges = wire_pending(pending, 5, EdgeKind.NORMAL)

        assert len(edges) == 2
        assert all(e.target == 5 for e in edges)

    def test_empty_pending_returns_empty(self):
        assert wire_pending((), 5, EdgeKind.NORMAL) == ()

    def test_negative_target_returns_empty(self):
        assert wire_pending((PendingEdge(source_id=1),), -1, EdgeKind.NORMAL) == ()


class TestWireIds:
    """Tests for wire_ids()."""

    def test_wires_ids_to_target(self):
        edges = wire_ids((10, 20), 30, EdgeKind.EXCEPTION)

        assert len(edges) == 2
        assert {e.source for e in edges} == {10, 20}
        assert all(e.target == 30 for e in edges)
        assert all(e.kind == EdgeKind.EXCEPTION for e in edges)

    def test_empty_ids_returns_empty(self):
        assert wire_ids((), 5, EdgeKind.NORMAL) == ()


class TestChain:
    """Tests for chain()."""

    def test_chain_two_fragments(self):
        left = Fragment(entry=1, exits=frozenset({2}))
        right = Fragment(entry=3, exits=frozenset({4}))

        combined, edges = chain(left, right)

        assert combined.entry == 1
        assert combined.exits == frozenset({4})
        assert edges == (CFGEdge(source=2, target=3, kind=EdgeKind.NORMAL),)

    def test_chain_with_empty_left(self):
        right = Fragment(entry=3, exits=frozenset({4}))

        combined, edges = chain(EMPTY_FRAGMENT, right)

        assert combined is right
        assert edges == ()

    def test_chain_with_empty_right(self):
        left = Fragment(entry=1, exits=frozenset({2}))

        combined, edges = chain(left, EMPTY_FRAGMENT)

        assert combined.entry == 1
        assert combined.exits == frozenset({2})
        assert edges == ()

    def test_chain_both_empty(self):
        combined, edges = chain(EMPTY_FRAGMENT, EMPTY_FRAGMENT)

        assert combined.entry == -1
        assert edges == ()

    def test_chain_merges_pending(self):
        left = Fragment(
            entry=1,
            exits=frozenset({2}),
            pending_breaks=(PendingEdge(source_id=10),),
            pending_returns=(20,),
        )
        right = Fragment(
            entry=3,
            exits=frozenset({4}),
            pending_continues=(PendingEdge(source_id=30),),
            pending_throws=(40,),
        )

        combined, _ = chain(left, right)

        assert combined.pending_breaks == (PendingEdge(source_id=10),)
        assert combined.pending_returns == (20,)
        assert combined.pending_continues == (PendingEdge(source_id=30),)
        assert combined.pending_throws == (40,)

    def test_chain_multiple_exits(self):
        left = Fragment(entry=1, exits=frozenset({2, 3}))
        right = Fragment(entry=5, exits=frozenset({6}))

        combined, edges = chain(left, right)

        assert len(edges) == 2
        assert {e.source for e in edges} == {2, 3}
        assert all(e.target == 5 for e in edges)


class TestMergeExits:
    """Tests for merge_exits()."""

    def test_merges_exits_from_multiple_fragments(self):
        f1 = Fragment(entry=1, exits=frozenset({2, 3}))
        f2 = Fragment(entry=4, exits=frozenset({5}))

        assert merge_exits(f1, f2) == frozenset({2, 3, 5})

    def test_skips_empty_fragments(self):
        f1 = Fragment(entry=1, exits=frozenset({2}))

        assert merge_exits(f1, EMPTY_FRAGMENT) == frozenset({2})

    def test_all_empty(self):
        assert merge_exits(EMPTY_FRAGMENT) == frozenset()


class TestMergePending:
    """Tests for merge_pending()."""

    def test_merges_pending_from_multiple_fragments(self):
        f1 = Fragment(
            entry=1,
            exits=frozenset({2}),
            pending_breaks=(PendingEdge(source_id=10),),
        )
        f2 = Fragment(
            entry=3,
            exits=frozenset({4}),
            pending_throws=(50,),
        )

        result = merge_pending(f1, f2)

        assert result.entry == -1
        assert result.exits == frozenset()
        assert result.pending_breaks == (PendingEdge(source_id=10),)
        assert result.pending_throws == (50,)


class TestMakeAtomicAndTerminals:
    """Tests for make_atomic and make_terminal_* functions."""

    def test_make_atomic(self):
        frag = make_atomic(5)
        assert frag.entry == 5
        assert frag.exits == frozenset({5})
        assert frag.pending_breaks == ()

    def test_make_terminal_return(self):
        frag = make_terminal_return(7)
        assert frag.entry == 7
        assert frag.exits == frozenset()
        assert frag.pending_returns == (7,)

    def test_make_terminal_throw(self):
        frag = make_terminal_throw(8)
        assert frag.entry == 8
        assert frag.exits == frozenset()
        assert frag.pending_throws == (8,)

    def test_make_terminal_break_unlabeled(self):
        frag = make_terminal_break(9)
        assert frag.entry == 9
        assert frag.exits == frozenset()
        assert frag.pending_breaks == (PendingEdge(source_id=9, label=""),)

    def test_make_terminal_break_labeled(self):
        frag = make_terminal_break(9, label="outer")
        assert frag.pending_breaks[0].label == "outer"

    def test_make_terminal_continue_unlabeled(self):
        frag = make_terminal_continue(10)
        assert frag.entry == 10
        assert frag.exits == frozenset()
        assert frag.pending_continues == (PendingEdge(source_id=10, label=""),)

    def test_make_terminal_continue_labeled(self):
        frag = make_terminal_continue(10, label="loop")
        assert frag.pending_continues[0].label == "loop"


class TestResolveBreaks:
    """Tests for resolve_breaks()."""

    def test_resolve_unlabeled_breaks(self):
        frag = Fragment(
            entry=1,
            exits=frozenset({5}),
            pending_breaks=(
                PendingEdge(source_id=10),
                PendingEdge(source_id=11),
            ),
        )

        resolved, edges = resolve_breaks(frag, frozenset())

        assert resolved.exits == frozenset({5, 10, 11})
        assert resolved.pending_breaks == ()
        assert edges == ()

    def test_resolve_labeled_breaks_only_matching(self):
        frag = Fragment(
            entry=1,
            exits=frozenset({5}),
            pending_breaks=(
                PendingEdge(source_id=10, label="outer"),
                PendingEdge(source_id=11),
                PendingEdge(source_id=12, label="inner"),
            ),
        )

        resolved, _ = resolve_breaks(frag, frozenset(), label="outer")

        assert 10 in resolved.exits
        assert 11 not in resolved.exits  # unlabeled, not matching "outer"
        assert 12 not in resolved.exits  # "inner" != "outer"
        assert len(resolved.pending_breaks) == 2

    def test_no_matching_breaks(self):
        frag = Fragment(
            entry=1,
            exits=frozenset({5}),
            pending_breaks=(PendingEdge(source_id=10, label="other"),),
        )

        resolved, _ = resolve_breaks(frag, frozenset())

        assert resolved.exits == frozenset({5})
        assert len(resolved.pending_breaks) == 1


class TestResolveContinues:
    """Tests for resolve_continues()."""

    def test_resolve_unlabeled_continues(self):
        frag = Fragment(
            entry=1,
            exits=frozenset({5}),
            pending_continues=(
                PendingEdge(source_id=20),
                PendingEdge(source_id=21),
            ),
        )

        resolved, edges = resolve_continues(frag, target=3)

        assert resolved.pending_continues == ()
        assert len(edges) == 2
        assert all(e.target == 3 for e in edges)
        assert all(e.kind == EdgeKind.BACK for e in edges)

    def test_resolve_labeled_continues(self):
        frag = Fragment(
            entry=1,
            exits=frozenset({5}),
            pending_continues=(
                PendingEdge(source_id=20, label="loop"),
                PendingEdge(source_id=21),
            ),
        )

        resolved, edges = resolve_continues(frag, target=3, label="loop")

        assert len(edges) == 1
        assert edges[0].source == 20
        assert len(resolved.pending_continues) == 1
        assert resolved.pending_continues[0].source_id == 21
