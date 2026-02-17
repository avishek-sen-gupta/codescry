"""Per-role handler functions for CFG fragment construction.

Each handler takes a tree-sitter node and its spec, builds child fragments
recursively, then wires them according to the role's control flow semantics.
"""

from dataclasses import dataclass, field
from typing import Callable

from repo_surveyor.cfg_constructor.cfg_types import (
    CFGEdge,
    CFGNode,
    EMPTY_FRAGMENT,
    EdgeKind,
    Fragment,
    PendingEdge,
)
from repo_surveyor.cfg_constructor.child_resolver import (
    resolve_body_children,
    resolve_semantic_slot,
)
from repo_surveyor.cfg_constructor.fragment_ops import (
    chain,
    make_atomic,
    make_terminal_break,
    make_terminal_continue,
    make_terminal_return,
    make_terminal_throw,
    merge_exits,
    resolve_breaks,
    resolve_continues,
    wire,
    wire_ids,
)
from repo_surveyor.cfg_constructor.ts_protocol import TSNode
from repo_surveyor.cfg_constructor.types import (
    ControlFlowRole,
    LanguageCFGSpec,
    NodeCFGSpec,
    SemanticSlot,
)

_SNIPPET_LIMIT = 80


@dataclass(frozen=True)
class LabelTarget:
    """Resolution target for labeled break/continue statements."""

    break_target: frozenset[int]
    continue_target: int


@dataclass
class NodeFactory:
    """Allocates monotonically increasing node IDs and builds CFGNode instances."""

    _counter: int = 0
    _nodes: list[CFGNode] = field(default_factory=list)

    def create(self, ts_node: TSNode, role: ControlFlowRole) -> CFGNode:
        """Create a new CFGNode from a tree-sitter node."""
        node_id = self._counter
        self._counter += 1
        text = ts_node.text or b""
        snippet = text[:_SNIPPET_LIMIT].decode("utf-8", errors="replace")
        cfg_node = CFGNode(
            id=node_id,
            node_type=ts_node.type,
            role=role,
            start_point=(ts_node.start_point.row, ts_node.start_point.column),
            end_point=(ts_node.end_point.row, ts_node.end_point.column),
            text_snippet=snippet,
        )
        self._nodes.append(cfg_node)
        return cfg_node

    def create_sentinel(self, label: str) -> CFGNode:
        """Create a synthetic ENTRY/EXIT sentinel node."""
        node_id = self._counter
        self._counter += 1
        cfg_node = CFGNode(
            id=node_id,
            node_type=label,
            role=ControlFlowRole.LEAF,
            start_point=(0, 0),
            end_point=(0, 0),
            text_snippet="",
        )
        self._nodes.append(cfg_node)
        return cfg_node

    @property
    def all_nodes(self) -> tuple[CFGNode, ...]:
        return tuple(self._nodes)


# Type alias for the recursive walker callback injected into handlers.
BuildFn = Callable[[TSNode], tuple[Fragment, tuple[CFGEdge, ...]]]


def handle_leaf(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    _build: BuildFn,
    _lang_spec: LanguageCFGSpec,
    _label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """LEAF / CALL: atomic node, entry == exit == self."""
    cfg_node = factory.create(ts_node, spec.role)
    return make_atomic(cfg_node.id), ()


def handle_return(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    _build: BuildFn,
    _lang_spec: LanguageCFGSpec,
    _label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """RETURN: entry=self, no exits, pending return."""
    cfg_node = factory.create(ts_node, ControlFlowRole.RETURN)
    return make_terminal_return(cfg_node.id), ()


def handle_throw(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    _build: BuildFn,
    _lang_spec: LanguageCFGSpec,
    _label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """THROW: entry=self, no exits, pending throw."""
    cfg_node = factory.create(ts_node, ControlFlowRole.THROW)
    return make_terminal_throw(cfg_node.id), ()


def _extract_label(ts_node: TSNode) -> str:
    """Extract the label text from a break/continue statement's first named child."""
    children = ts_node.named_children
    if not children:
        return ""
    label_node = children[0]
    text = label_node.text or b""
    return text.decode("utf-8", errors="replace").strip()


def handle_break(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    _build: BuildFn,
    _lang_spec: LanguageCFGSpec,
    _label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """BREAK: entry=self, no exits, pending break with optional label."""
    cfg_node = factory.create(ts_node, ControlFlowRole.BREAK)
    label = _extract_label(ts_node)
    return make_terminal_break(cfg_node.id, label), ()


def handle_continue(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    _build: BuildFn,
    _lang_spec: LanguageCFGSpec,
    _label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """CONTINUE: entry=self, no exits, pending continue with optional label."""
    cfg_node = factory.create(ts_node, ControlFlowRole.CONTINUE)
    label = _extract_label(ts_node)
    return make_terminal_continue(cfg_node.id, label), ()


def handle_sequence(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    build: BuildFn,
    lang_spec: LanguageCFGSpec,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """SEQUENCE: chain children left-to-right, stop after dead code."""
    children = resolve_body_children(ts_node, spec)
    if not children:
        return EMPTY_FRAGMENT, ()

    all_edges: list[CFGEdge] = []
    current = EMPTY_FRAGMENT

    for child in children:
        child_frag, child_edges = build(child)
        all_edges.extend(child_edges)

        combined, chain_edges = chain(current, child_frag)
        all_edges.extend(chain_edges)
        current = combined

        # Stop after a node with no exits (dead code after return/throw/break/continue)
        if child_frag.entry != -1 and not child_frag.exits:
            break

    return current, tuple(all_edges)


def handle_branch(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    build: BuildFn,
    lang_spec: LanguageCFGSpec,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """BRANCH (if/else, ternary): condition -> TRUE consequence, FALSE alternative."""
    all_edges: list[CFGEdge] = []

    cond_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.CONDITION)
    cons_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.CONSEQUENCE)
    alt_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.ALTERNATIVE)

    cond_frag, cond_edges = build(cond_node) if cond_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(cond_edges)

    cons_frag, cons_edges = build(cons_node) if cons_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(cons_edges)

    alt_frag, alt_edges = build(alt_node) if alt_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(alt_edges)

    # Wire condition -> consequence (TRUE)
    if cons_frag.entry != -1:
        all_edges.extend(wire(cond_frag.exits, cons_frag.entry, EdgeKind.TRUE))

    # Wire condition -> alternative (FALSE)
    if alt_frag.entry != -1:
        all_edges.extend(wire(cond_frag.exits, alt_frag.entry, EdgeKind.FALSE))

    # Determine exits
    exits: frozenset[int] = frozenset()
    if cons_frag.entry != -1:
        exits = exits | cons_frag.exits
    else:
        # No consequence — condition exits are also branch exits (TRUE path)
        exits = exits | cond_frag.exits

    if alt_frag.entry != -1:
        exits = exits | alt_frag.exits
    else:
        # No alternative — condition exits go to branch exits (FALSE path)
        exits = exits | cond_frag.exits

    entry = cond_frag.entry if cond_frag.entry != -1 else -1

    pending_breaks = (
        cond_frag.pending_breaks + cons_frag.pending_breaks + alt_frag.pending_breaks
    )
    pending_continues = (
        cond_frag.pending_continues
        + cons_frag.pending_continues
        + alt_frag.pending_continues
    )
    pending_throws = (
        cond_frag.pending_throws + cons_frag.pending_throws + alt_frag.pending_throws
    )
    pending_returns = (
        cond_frag.pending_returns + cons_frag.pending_returns + alt_frag.pending_returns
    )

    frag = Fragment(
        entry=entry,
        exits=exits,
        pending_breaks=pending_breaks,
        pending_continues=pending_continues,
        pending_throws=pending_throws,
        pending_returns=pending_returns,
    )
    return frag, tuple(all_edges)


def handle_loop(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    build: BuildFn,
    lang_spec: LanguageCFGSpec,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """LOOP (for/while): [init ->] condition ->(TRUE) body [-> update -> condition(BACK)].

    condition ->(FALSE) exits.
    """
    all_edges: list[CFGEdge] = []

    init_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.INITIALIZER)
    cond_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.CONDITION)
    body_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.BODY)
    update_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.UPDATE)

    init_frag, init_edges = build(init_node) if init_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(init_edges)

    cond_frag, cond_edges = build(cond_node) if cond_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(cond_edges)

    body_frag, body_edges = build(body_node) if body_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(body_edges)

    update_frag, update_edges = (
        build(update_node) if update_node else (EMPTY_FRAGMENT, ())
    )
    all_edges.extend(update_edges)

    # Wire init -> condition
    if init_frag.entry != -1 and cond_frag.entry != -1:
        all_edges.extend(wire(init_frag.exits, cond_frag.entry, EdgeKind.NORMAL))

    # Wire condition -> body (TRUE)
    if cond_frag.entry != -1 and body_frag.entry != -1:
        all_edges.extend(wire(cond_frag.exits, body_frag.entry, EdgeKind.TRUE))

    # Determine back-edge target: update if present, else condition
    back_target = update_frag.entry if update_frag.entry != -1 else cond_frag.entry

    # Wire body -> update (or back to condition)
    if body_frag.entry != -1:
        if update_frag.entry != -1:
            all_edges.extend(wire(body_frag.exits, update_frag.entry, EdgeKind.NORMAL))
            # Wire update -> condition (BACK)
            if cond_frag.entry != -1:
                all_edges.extend(
                    wire(update_frag.exits, cond_frag.entry, EdgeKind.BACK)
                )
        elif cond_frag.entry != -1:
            all_edges.extend(wire(body_frag.exits, cond_frag.entry, EdgeKind.BACK))

    # For-each loops without condition: body loops back to entry (BACK)
    if cond_frag.entry == -1 and body_frag.entry != -1:
        all_edges.extend(wire(body_frag.exits, body_frag.entry, EdgeKind.BACK))

    # Determine entry
    entry = (
        init_frag.entry
        if init_frag.entry != -1
        else (cond_frag.entry if cond_frag.entry != -1 else body_frag.entry)
    )

    # Exits: condition FALSE (if condition exists), or body exits for for-each
    loop_exits = cond_frag.exits if cond_frag.entry != -1 else body_frag.exits

    # Collect pending edges
    pending_breaks = body_frag.pending_breaks + update_frag.pending_breaks
    pending_continues = body_frag.pending_continues + update_frag.pending_continues
    pending_throws = (
        init_frag.pending_throws
        + cond_frag.pending_throws
        + body_frag.pending_throws
        + update_frag.pending_throws
    )
    pending_returns = (
        init_frag.pending_returns
        + cond_frag.pending_returns
        + body_frag.pending_returns
        + update_frag.pending_returns
    )

    frag = Fragment(
        entry=entry,
        exits=loop_exits,
        pending_breaks=pending_breaks,
        pending_continues=pending_continues,
        pending_throws=pending_throws,
        pending_returns=pending_returns,
    )

    # Resolve continues -> back_target
    frag, cont_edges = resolve_continues(frag, back_target)
    all_edges.extend(cont_edges)

    # Resolve breaks -> exits
    frag, _ = resolve_breaks(frag, frag.exits)

    return frag, tuple(all_edges)


def handle_loop_post_condition(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    build: BuildFn,
    lang_spec: LanguageCFGSpec,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """LOOP_POST_CONDITION (do-while): body -> condition ->(TRUE) body(BACK).

    condition ->(FALSE) exits.
    """
    all_edges: list[CFGEdge] = []

    body_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.BODY)
    cond_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.CONDITION)

    body_frag, body_edges = build(body_node) if body_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(body_edges)

    cond_frag, cond_edges = build(cond_node) if cond_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(cond_edges)

    # Wire body -> condition
    if body_frag.entry != -1 and cond_frag.entry != -1:
        all_edges.extend(wire(body_frag.exits, cond_frag.entry, EdgeKind.NORMAL))

    # Wire condition -> body (TRUE, BACK)
    if cond_frag.entry != -1 and body_frag.entry != -1:
        all_edges.extend(wire(cond_frag.exits, body_frag.entry, EdgeKind.BACK))

    entry = body_frag.entry if body_frag.entry != -1 else cond_frag.entry

    # Exits: condition FALSE
    loop_exits = cond_frag.exits if cond_frag.entry != -1 else frozenset()

    pending_throws = body_frag.pending_throws + cond_frag.pending_throws
    pending_returns = body_frag.pending_returns + cond_frag.pending_returns

    frag = Fragment(
        entry=entry,
        exits=loop_exits,
        pending_breaks=body_frag.pending_breaks,
        pending_continues=body_frag.pending_continues,
        pending_throws=pending_throws,
        pending_returns=pending_returns,
    )

    # Resolve continues -> condition (back target)
    back_target = cond_frag.entry if cond_frag.entry != -1 else body_frag.entry
    frag, cont_edges = resolve_continues(frag, back_target)
    all_edges.extend(cont_edges)

    # Resolve breaks -> exits
    frag, _ = resolve_breaks(frag, frag.exits)

    return frag, tuple(all_edges)


def handle_switch(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    build: BuildFn,
    lang_spec: LanguageCFGSpec,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """SWITCH: value -> each arm.

    With fallthrough: arms wired sequentially (FALLTHROUGH).
    Without fallthrough: arms are independent.
    """
    all_edges: list[CFGEdge] = []

    value_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.VALUE)
    value_frag, value_edges = build(value_node) if value_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(value_edges)

    # The body contains the case arms
    body_children = resolve_body_children(ts_node, spec)

    arm_frags: list[Fragment] = []
    for child in body_children:
        arm_frag, arm_edges = build(child)
        all_edges.extend(arm_edges)
        arm_frags.append(arm_frag)

    fallthrough = lang_spec.switch_fallthrough

    if fallthrough:
        # Wire value -> first arm entry
        if arm_frags and arm_frags[0].entry != -1 and value_frag.entry != -1:
            all_edges.extend(
                wire(value_frag.exits, arm_frags[0].entry, EdgeKind.NORMAL)
            )
        # Also wire value -> each arm entry (jump to matching case)
        for arm_frag in arm_frags[1:]:
            if arm_frag.entry != -1 and value_frag.entry != -1:
                all_edges.extend(
                    wire(value_frag.exits, arm_frag.entry, EdgeKind.NORMAL)
                )
        # Wire arms sequentially (FALLTHROUGH)
        for i in range(len(arm_frags) - 1):
            left = arm_frags[i]
            right = arm_frags[i + 1]
            if left.entry != -1 and right.entry != -1:
                all_edges.extend(wire(left.exits, right.entry, EdgeKind.FALLTHROUGH))
    else:
        # No fallthrough: wire value -> each arm independently
        for arm_frag in arm_frags:
            if arm_frag.entry != -1 and value_frag.entry != -1:
                all_edges.extend(
                    wire(value_frag.exits, arm_frag.entry, EdgeKind.NORMAL)
                )

    # Collect exits and pending
    arm_exits = merge_exits(*arm_frags) if arm_frags else frozenset()
    # If no arms, value exits become switch exits
    if not arm_frags:
        arm_exits = value_frag.exits

    pending_breaks: tuple[PendingEdge, ...] = ()
    pending_continues: tuple[PendingEdge, ...] = ()
    pending_throws: tuple[int, ...] = value_frag.pending_throws
    pending_returns: tuple[int, ...] = value_frag.pending_returns

    for arm_frag in arm_frags:
        pending_breaks = pending_breaks + arm_frag.pending_breaks
        pending_continues = pending_continues + arm_frag.pending_continues
        pending_throws = pending_throws + arm_frag.pending_throws
        pending_returns = pending_returns + arm_frag.pending_returns

    entry = value_frag.entry if value_frag.entry != -1 else -1

    frag = Fragment(
        entry=entry,
        exits=arm_exits,
        pending_breaks=pending_breaks,
        pending_continues=pending_continues,
        pending_throws=pending_throws,
        pending_returns=pending_returns,
    )

    # Resolve breaks -> exits (switch breaks)
    frag, _ = resolve_breaks(frag, frag.exits)

    return frag, tuple(all_edges)


def handle_try(
    ts_node: TSNode,
    spec: NodeCFGSpec,
    factory: NodeFactory,
    build: BuildFn,
    lang_spec: LanguageCFGSpec,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """TRY (funnel model): body -> handler (via throws), body+handler -> finally.

    Finally intercepts ALL outgoing paths.
    """
    all_edges: list[CFGEdge] = []

    body_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.BODY)
    handler_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.HANDLER)
    finalizer_node = resolve_semantic_slot(ts_node, spec, SemanticSlot.FINALIZER)

    body_frag, body_edges = build(body_node) if body_node else (EMPTY_FRAGMENT, ())
    all_edges.extend(body_edges)

    handler_frag, handler_edges = (
        build(handler_node) if handler_node else (EMPTY_FRAGMENT, ())
    )
    all_edges.extend(handler_edges)

    finally_frag, finally_edges = (
        build(finalizer_node) if finalizer_node else (EMPTY_FRAGMENT, ())
    )
    all_edges.extend(finally_edges)

    # Wire pending throws from body -> handler entry (EXCEPTION)
    if handler_frag.entry != -1 and body_frag.pending_throws:
        all_edges.extend(
            wire_ids(body_frag.pending_throws, handler_frag.entry, EdgeKind.EXCEPTION)
        )
        resolved_throws: tuple[int, ...] = ()
    else:
        resolved_throws = body_frag.pending_throws

    # Normal exits: body.exits + handler.exits
    pre_finally_exits = merge_exits(body_frag, handler_frag)

    # Collect all pending from body + handler
    combined_breaks = body_frag.pending_breaks + handler_frag.pending_breaks
    combined_continues = body_frag.pending_continues + handler_frag.pending_continues
    combined_throws = resolved_throws + handler_frag.pending_throws
    combined_returns = body_frag.pending_returns + handler_frag.pending_returns

    if finally_frag.entry != -1:
        # Wire normal exits -> finally
        all_edges.extend(wire(pre_finally_exits, finally_frag.entry, EdgeKind.NORMAL))

        # Wire pending returns through finally
        if combined_returns:
            all_edges.extend(
                wire_ids(combined_returns, finally_frag.entry, EdgeKind.NORMAL)
            )

        # Wire pending breaks through finally
        if combined_breaks:
            all_edges.extend(
                wire_ids(
                    tuple(pe.source_id for pe in combined_breaks),
                    finally_frag.entry,
                    EdgeKind.NORMAL,
                )
            )

        # Wire pending continues through finally
        if combined_continues:
            all_edges.extend(
                wire_ids(
                    tuple(pe.source_id for pe in combined_continues),
                    finally_frag.entry,
                    EdgeKind.NORMAL,
                )
            )

        # Wire pending throws through finally
        if combined_throws:
            all_edges.extend(
                wire_ids(combined_throws, finally_frag.entry, EdgeKind.NORMAL)
            )

        # Re-pend everything from finally's exits
        frag = Fragment(
            entry=body_frag.entry if body_frag.entry != -1 else handler_frag.entry,
            exits=finally_frag.exits,
            pending_breaks=combined_breaks,
            pending_continues=combined_continues,
            pending_throws=combined_throws,
            pending_returns=combined_returns,
        )
    else:
        frag = Fragment(
            entry=body_frag.entry if body_frag.entry != -1 else handler_frag.entry,
            exits=pre_finally_exits,
            pending_breaks=combined_breaks,
            pending_continues=combined_continues,
            pending_throws=combined_throws,
            pending_returns=combined_returns,
        )

    return frag, tuple(all_edges)


# Dispatch table mapping roles to handler functions.
ROLE_HANDLERS: dict[ControlFlowRole, Callable] = {
    ControlFlowRole.LEAF: handle_leaf,
    ControlFlowRole.CALL: handle_leaf,
    ControlFlowRole.RETURN: handle_return,
    ControlFlowRole.THROW: handle_throw,
    ControlFlowRole.BREAK: handle_break,
    ControlFlowRole.CONTINUE: handle_continue,
    ControlFlowRole.SEQUENCE: handle_sequence,
    ControlFlowRole.BRANCH: handle_branch,
    ControlFlowRole.LOOP: handle_loop,
    ControlFlowRole.LOOP_POST_CONDITION: handle_loop_post_condition,
    ControlFlowRole.SWITCH: handle_switch,
    ControlFlowRole.TRY: handle_try,
}
