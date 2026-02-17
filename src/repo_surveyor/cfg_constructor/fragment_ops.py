"""Pure functions for wiring and merging Fragment objects.

All functions are side-effect-free and work on immutable Fragment values,
producing new edges and Fragments as output.
"""

from repo_surveyor.cfg_constructor.cfg_types import (
    CFGEdge,
    EMPTY_FRAGMENT,
    EdgeKind,
    Fragment,
    PendingEdge,
)


def wire(sources: frozenset[int], target: int, kind: EdgeKind) -> tuple[CFGEdge, ...]:
    """Create edges from each *source* to *target* with the given *kind*.

    Returns an empty tuple if *sources* is empty or *target* is -1.
    """
    if not sources or target == -1:
        return ()
    return tuple(CFGEdge(source=s, target=target, kind=kind) for s in sources)


def wire_pending(
    pending: tuple[PendingEdge, ...], target: int, kind: EdgeKind
) -> tuple[CFGEdge, ...]:
    """Wire pending edges to a single *target*."""
    if not pending or target == -1:
        return ()
    return tuple(
        CFGEdge(source=pe.source_id, target=target, kind=kind) for pe in pending
    )


def wire_ids(
    source_ids: tuple[int, ...], target: int, kind: EdgeKind
) -> tuple[CFGEdge, ...]:
    """Wire a tuple of source IDs to a single *target*."""
    if not source_ids or target == -1:
        return ()
    return tuple(CFGEdge(source=s, target=target, kind=kind) for s in source_ids)


def chain(
    left: Fragment, right: Fragment, kind: EdgeKind = EdgeKind.NORMAL
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """Wire *left*'s exits to *right*'s entry, producing a combined Fragment.

    If either fragment is empty, the other is returned unchanged with no
    new edges.  The combined Fragment inherits pending edges from both sides.

    Returns ``(combined_fragment, new_edges)``.
    """
    if left is EMPTY_FRAGMENT or left.entry == -1:
        return right, ()
    if right is EMPTY_FRAGMENT or right.entry == -1:
        return _merge_pending(left, right), ()

    new_edges = wire(left.exits, right.entry, kind)
    combined = Fragment(
        entry=left.entry,
        exits=right.exits,
        pending_breaks=left.pending_breaks + right.pending_breaks,
        pending_continues=left.pending_continues + right.pending_continues,
        pending_throws=left.pending_throws + right.pending_throws,
        pending_returns=left.pending_returns + right.pending_returns,
    )
    return combined, new_edges


def merge_exits(*fragments: Fragment) -> frozenset[int]:
    """Union the exits of multiple fragments, filtering out empty ones."""
    result: set[int] = set()
    for f in fragments:
        if f.entry != -1:
            result.update(f.exits)
    return frozenset(result)


def merge_pending(*fragments: Fragment) -> Fragment:
    """Merge pending edges from multiple fragments into a single collection.

    The returned Fragment has ``entry=-1`` and ``exits=frozenset()`` —
    it is only used as a container for accumulated pending edges.
    """
    return Fragment(
        entry=-1,
        exits=frozenset(),
        pending_breaks=_cat_breaks(fragments),
        pending_continues=_cat_continues(fragments),
        pending_throws=_cat_throws(fragments),
        pending_returns=_cat_returns(fragments),
    )


def make_atomic(node_id: int) -> Fragment:
    """Create a Fragment for an atomic (LEAF/CALL) node: entry == exit == self."""
    return Fragment(entry=node_id, exits=frozenset({node_id}))


def make_terminal_return(node_id: int) -> Fragment:
    """Create a Fragment for a RETURN node: entry=self, no exits, pending return."""
    return Fragment(
        entry=node_id,
        exits=frozenset(),
        pending_returns=(node_id,),
    )


def make_terminal_throw(node_id: int) -> Fragment:
    """Create a Fragment for a THROW node: entry=self, no exits, pending throw."""
    return Fragment(
        entry=node_id,
        exits=frozenset(),
        pending_throws=(node_id,),
    )


def make_terminal_break(node_id: int, label: str = "") -> Fragment:
    """Create a Fragment for a BREAK node: entry=self, no exits, pending break."""
    return Fragment(
        entry=node_id,
        exits=frozenset(),
        pending_breaks=(PendingEdge(source_id=node_id, label=label),),
    )


def make_terminal_continue(node_id: int, label: str = "") -> Fragment:
    """Create a Fragment for a CONTINUE node: entry=self, no exits, pending continue."""
    return Fragment(
        entry=node_id,
        exits=frozenset(),
        pending_continues=(PendingEdge(source_id=node_id, label=label),),
    )


def resolve_breaks(
    fragment: Fragment, target: frozenset[int], label: str = ""
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """Resolve pending breaks (matching *label*) by adding their sources to *target*.

    Unlabeled breaks match when *label* is empty.  Labeled breaks match
    only when the label is equal.  Non-matching breaks are preserved.

    Returns ``(fragment_without_resolved_breaks, [])`` — resolved breaks
    become additional exits rather than edges, since the break target is
    the enclosing construct's exit set.
    """
    matching = tuple(pe for pe in fragment.pending_breaks if pe.label == label)
    remaining = tuple(pe for pe in fragment.pending_breaks if pe.label != label)
    new_exits = fragment.exits | frozenset(pe.source_id for pe in matching)
    return (
        Fragment(
            entry=fragment.entry,
            exits=new_exits,
            pending_breaks=remaining,
            pending_continues=fragment.pending_continues,
            pending_throws=fragment.pending_throws,
            pending_returns=fragment.pending_returns,
        ),
        (),
    )


def resolve_continues(
    fragment: Fragment, target: int, label: str = ""
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """Resolve pending continues (matching *label*) by wiring to *target*.

    Returns ``(fragment_without_resolved_continues, new_edges)``.
    """
    matching = tuple(pe for pe in fragment.pending_continues if pe.label == label)
    remaining = tuple(pe for pe in fragment.pending_continues if pe.label != label)
    new_edges = tuple(
        CFGEdge(source=pe.source_id, target=target, kind=EdgeKind.BACK)
        for pe in matching
    )
    return (
        Fragment(
            entry=fragment.entry,
            exits=fragment.exits,
            pending_breaks=fragment.pending_breaks,
            pending_continues=remaining,
            pending_throws=fragment.pending_throws,
            pending_returns=fragment.pending_returns,
        ),
        new_edges,
    )


def _merge_pending(base: Fragment, extra: Fragment) -> Fragment:
    """Merge pending edges from *extra* into *base*, keeping base's entry/exits."""
    return Fragment(
        entry=base.entry,
        exits=base.exits,
        pending_breaks=base.pending_breaks + extra.pending_breaks,
        pending_continues=base.pending_continues + extra.pending_continues,
        pending_throws=base.pending_throws + extra.pending_throws,
        pending_returns=base.pending_returns + extra.pending_returns,
    )


def _cat_breaks(fragments: tuple[Fragment, ...]) -> tuple[PendingEdge, ...]:
    result: list[PendingEdge] = []
    for f in fragments:
        result.extend(f.pending_breaks)
    return tuple(result)


def _cat_continues(fragments: tuple[Fragment, ...]) -> tuple[PendingEdge, ...]:
    result: list[PendingEdge] = []
    for f in fragments:
        result.extend(f.pending_continues)
    return tuple(result)


def _cat_throws(fragments: tuple[Fragment, ...]) -> tuple[int, ...]:
    result: list[int] = []
    for f in fragments:
        result.extend(f.pending_throws)
    return tuple(result)


def _cat_returns(fragments: tuple[Fragment, ...]) -> tuple[int, ...]:
    result: list[int] = []
    for f in fragments:
        result.extend(f.pending_returns)
    return tuple(result)
