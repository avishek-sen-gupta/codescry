"""Tests for role_handlers using stub tree-sitter nodes."""

from dataclasses import dataclass, field

import pytest

from repo_surveyor.cfg_constructor.cfg_types import (
    CFGEdge,
    EMPTY_FRAGMENT,
    EdgeKind,
    Fragment,
    PendingEdge,
)
from repo_surveyor.cfg_constructor.role_handlers import (
    BuildFn,
    NodeFactory,
    handle_branch,
    handle_break,
    handle_continue,
    handle_leaf,
    handle_loop,
    handle_loop_post_condition,
    handle_return,
    handle_sequence,
    handle_switch,
    handle_throw,
    handle_try,
)
from repo_surveyor.cfg_constructor.types import (
    ControlFlowRole,
    FieldMapping,
    LanguageCFGSpec,
    NodeCFGSpec,
    SemanticSlot,
)
from repo_surveyor.integration_patterns.types import Language


@dataclass
class StubPoint:
    row: int = 0
    column: int = 0


@dataclass
class StubTSNode:
    type: str = "stub"
    children: list["StubTSNode"] = field(default_factory=list)
    named_children: list["StubTSNode"] = field(default_factory=list)
    start_point: StubPoint = field(default_factory=StubPoint)
    end_point: StubPoint = field(default_factory=StubPoint)
    text: bytes = b""
    _fields: dict[str, "StubTSNode"] = field(default_factory=dict)

    def child_by_field_name(self, name: str) -> "StubTSNode | None":
        return self._fields.get(name)


def _make_lang_spec(fallthrough: bool = False) -> LanguageCFGSpec:
    return LanguageCFGSpec(
        language=Language.JAVA,
        node_specs={},
        switch_fallthrough=fallthrough,
    )


def _leaf_spec() -> NodeCFGSpec:
    return NodeCFGSpec(role=ControlFlowRole.LEAF)


def _seq_spec() -> NodeCFGSpec:
    return NodeCFGSpec(role=ControlFlowRole.SEQUENCE)


def _branch_spec() -> NodeCFGSpec:
    return NodeCFGSpec(
        role=ControlFlowRole.BRANCH,
        field_mapping=FieldMapping(
            slots={
                SemanticSlot.CONDITION: "condition",
                SemanticSlot.CONSEQUENCE: "consequence",
                SemanticSlot.ALTERNATIVE: "alternative",
            }
        ),
    )


def _loop_spec() -> NodeCFGSpec:
    return NodeCFGSpec(
        role=ControlFlowRole.LOOP,
        field_mapping=FieldMapping(
            slots={
                SemanticSlot.CONDITION: "condition",
                SemanticSlot.BODY: "body",
            }
        ),
    )


def _do_while_spec() -> NodeCFGSpec:
    return NodeCFGSpec(
        role=ControlFlowRole.LOOP_POST_CONDITION,
        field_mapping=FieldMapping(
            slots={
                SemanticSlot.BODY: "body",
                SemanticSlot.CONDITION: "condition",
            }
        ),
    )


def _switch_spec() -> NodeCFGSpec:
    return NodeCFGSpec(
        role=ControlFlowRole.SWITCH,
        field_mapping=FieldMapping(
            slots={
                SemanticSlot.VALUE: "value",
                SemanticSlot.BODY: "body",
            }
        ),
    )


def _try_spec() -> NodeCFGSpec:
    return NodeCFGSpec(
        role=ControlFlowRole.TRY,
        field_mapping=FieldMapping(
            slots={
                SemanticSlot.BODY: "body",
                SemanticSlot.HANDLER: "handler",
                SemanticSlot.FINALIZER: "finalizer",
            }
        ),
    )


class TestNodeFactory:
    def test_creates_nodes_with_incrementing_ids(self):
        factory = NodeFactory()
        node1 = factory.create(
            StubTSNode(type="stmt", text=b"x = 1"), ControlFlowRole.LEAF
        )
        node2 = factory.create(
            StubTSNode(type="call", text=b"foo()"), ControlFlowRole.CALL
        )

        assert node1.id == 0
        assert node2.id == 1
        assert node1.node_type == "stmt"
        assert node2.node_type == "call"

    def test_sentinel_creation(self):
        factory = NodeFactory()
        entry = factory.create_sentinel("ENTRY")
        exit_node = factory.create_sentinel("EXIT")

        assert entry.id == 0
        assert exit_node.id == 1
        assert entry.node_type == "ENTRY"

    def test_all_nodes_tracked(self):
        factory = NodeFactory()
        factory.create(StubTSNode(type="a", text=b"a"), ControlFlowRole.LEAF)
        factory.create_sentinel("ENTRY")

        assert len(factory.all_nodes) == 2

    def test_text_snippet_truncation(self):
        factory = NodeFactory()
        long_text = b"x" * 200
        node = factory.create(
            StubTSNode(type="s", text=long_text), ControlFlowRole.LEAF
        )

        assert len(node.text_snippet) == 80


class TestHandleLeaf:
    def test_leaf_creates_atomic_fragment(self):
        factory = NodeFactory()
        ts_node = StubTSNode(type="identifier", text=b"x")

        frag, edges = handle_leaf(
            ts_node,
            _leaf_spec(),
            factory,
            lambda n: (EMPTY_FRAGMENT, ()),
            _make_lang_spec(),
            {},
        )

        assert frag.entry == 0
        assert frag.exits == frozenset({0})
        assert edges == ()


class TestHandleReturn:
    def test_return_creates_terminal_with_pending_return(self):
        factory = NodeFactory()
        ts_node = StubTSNode(type="return_statement", text=b"return x;")

        frag, edges = handle_return(
            ts_node,
            _leaf_spec(),
            factory,
            lambda n: (EMPTY_FRAGMENT, ()),
            _make_lang_spec(),
            {},
        )

        assert frag.entry == 0
        assert frag.exits == frozenset()
        assert frag.pending_returns == (0,)


class TestHandleThrow:
    def test_throw_creates_terminal_with_pending_throw(self):
        factory = NodeFactory()
        ts_node = StubTSNode(type="throw_statement", text=b"throw e;")

        frag, edges = handle_throw(
            ts_node,
            _leaf_spec(),
            factory,
            lambda n: (EMPTY_FRAGMENT, ()),
            _make_lang_spec(),
            {},
        )

        assert frag.entry == 0
        assert frag.exits == frozenset()
        assert frag.pending_throws == (0,)


class TestHandleBreak:
    def test_unlabeled_break(self):
        factory = NodeFactory()
        ts_node = StubTSNode(type="break_statement", text=b"break;")

        frag, _ = handle_break(
            ts_node,
            _leaf_spec(),
            factory,
            lambda n: (EMPTY_FRAGMENT, ()),
            _make_lang_spec(),
            {},
        )

        assert frag.exits == frozenset()
        assert frag.pending_breaks[0].label == ""

    def test_labeled_break(self):
        factory = NodeFactory()
        label_child = StubTSNode(type="identifier", text=b"outer")
        ts_node = StubTSNode(
            type="break_statement",
            text=b"break outer;",
            named_children=[label_child],
        )

        frag, _ = handle_break(
            ts_node,
            _leaf_spec(),
            factory,
            lambda n: (EMPTY_FRAGMENT, ()),
            _make_lang_spec(),
            {},
        )

        assert frag.pending_breaks[0].label == "outer"


class TestHandleContinue:
    def test_unlabeled_continue(self):
        factory = NodeFactory()
        ts_node = StubTSNode(type="continue_statement", text=b"continue;")

        frag, _ = handle_continue(
            ts_node,
            _leaf_spec(),
            factory,
            lambda n: (EMPTY_FRAGMENT, ()),
            _make_lang_spec(),
            {},
        )

        assert frag.exits == frozenset()
        assert frag.pending_continues[0].label == ""


class TestHandleSequence:
    def test_empty_sequence(self):
        factory = NodeFactory()
        ts_node = StubTSNode(type="block", named_children=[])

        frag, edges = handle_sequence(
            ts_node,
            _seq_spec(),
            factory,
            lambda n: (EMPTY_FRAGMENT, ()),
            _make_lang_spec(),
            {},
        )

        assert frag is EMPTY_FRAGMENT
        assert edges == ()

    def test_single_child(self):
        factory = NodeFactory()
        child = StubTSNode(type="stmt", text=b"x = 1")

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        ts_node = StubTSNode(type="block", named_children=[child])

        frag, _ = handle_sequence(
            ts_node, _seq_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        assert frag.entry == 0
        assert frag.exits == frozenset({0})

    def test_two_children_chained(self):
        factory = NodeFactory()
        child1 = StubTSNode(type="stmt1", text=b"a")
        child2 = StubTSNode(type="stmt2", text=b"b")

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        ts_node = StubTSNode(type="block", named_children=[child1, child2])

        frag, edges = handle_sequence(
            ts_node, _seq_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        assert frag.entry == 0
        assert frag.exits == frozenset({1})
        # Edge from 0 -> 1
        assert any(e.source == 0 and e.target == 1 for e in edges)

    def test_dead_code_after_return(self):
        factory = NodeFactory()
        ret_child = StubTSNode(type="return_statement", text=b"return;")
        dead_child = StubTSNode(type="stmt", text=b"dead")

        call_count = 0

        def build_fn(n):
            nonlocal call_count
            call_count += 1
            if n.type == "return_statement":
                node = factory.create(n, ControlFlowRole.RETURN)
                return (
                    Fragment(
                        entry=node.id, exits=frozenset(), pending_returns=(node.id,)
                    ),
                    (),
                )
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        ts_node = StubTSNode(type="block", named_children=[ret_child, dead_child])

        frag, _ = handle_sequence(
            ts_node, _seq_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        assert frag.exits == frozenset()
        assert frag.pending_returns == (0,)
        # dead_child should not be processed
        assert call_count == 1


class TestHandleBranch:
    def test_if_else(self):
        factory = NodeFactory()
        cond = StubTSNode(type="condition", text=b"x > 0")
        cons = StubTSNode(type="block", text=b"{ a; }")
        alt = StubTSNode(type="block", text=b"{ b; }")
        ts_node = StubTSNode(
            type="if_statement",
            _fields={"condition": cond, "consequence": cons, "alternative": alt},
        )

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_branch(
            ts_node, _branch_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        # Entry is condition (id=0), consequence (id=1), alternative (id=2)
        assert frag.entry == 0
        assert frag.exits == frozenset({1, 2})
        # Check TRUE and FALSE edges
        edge_kinds = {(e.source, e.target, e.kind) for e in edges}
        assert (0, 1, EdgeKind.TRUE) in edge_kinds
        assert (0, 2, EdgeKind.FALSE) in edge_kinds

    def test_if_without_else(self):
        factory = NodeFactory()
        cond = StubTSNode(type="condition", text=b"x > 0")
        cons = StubTSNode(type="block", text=b"{ a; }")
        ts_node = StubTSNode(
            type="if_statement",
            _fields={"condition": cond, "consequence": cons},
        )

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_branch(
            ts_node, _branch_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        # Without else, condition exits are also in the branch exits (FALSE path)
        assert 0 in frag.exits  # condition node as false-path exit
        assert 1 in frag.exits  # consequence exit


class TestHandleLoop:
    def test_while_loop(self):
        factory = NodeFactory()
        cond = StubTSNode(type="cond_expr", text=b"i < 10")
        body = StubTSNode(type="block", text=b"{ i++; }")
        ts_node = StubTSNode(
            type="while_statement",
            _fields={"condition": cond, "body": body},
        )

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_loop(
            ts_node, _loop_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        # Entry is condition (id=0), body (id=1)
        assert frag.entry == 0
        # Exits are condition exits (FALSE path)
        assert frag.exits == frozenset({0})
        edge_set = {(e.source, e.target, e.kind) for e in edges}
        assert (0, 1, EdgeKind.TRUE) in edge_set
        assert (1, 0, EdgeKind.BACK) in edge_set

    def test_while_loop_with_break(self):
        factory = NodeFactory()
        cond = StubTSNode(type="cond_expr", text=b"true")
        body = StubTSNode(type="block", text=b"{ break; }")
        ts_node = StubTSNode(
            type="while_statement",
            _fields={"condition": cond, "body": body},
        )

        def build_fn(n):
            if n.type == "cond_expr":
                node = factory.create(n, ControlFlowRole.LEAF)
                return Fragment(entry=node.id, exits=frozenset({node.id})), ()
            # Body has a break
            node = factory.create(n, ControlFlowRole.BREAK)
            return (
                Fragment(
                    entry=node.id,
                    exits=frozenset(),
                    pending_breaks=(PendingEdge(source_id=node.id),),
                ),
                (),
            )

        frag, _ = handle_loop(
            ts_node, _loop_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        # Break should be resolved to loop exits
        assert 1 in frag.exits  # break node becomes an exit
        assert frag.pending_breaks == ()


class TestHandleLoopPostCondition:
    def test_do_while(self):
        factory = NodeFactory()
        body = StubTSNode(type="block", text=b"{ x++; }")
        cond = StubTSNode(type="cond_expr", text=b"x < 10")
        ts_node = StubTSNode(
            type="do_statement",
            _fields={"body": body, "condition": cond},
        )

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_loop_post_condition(
            ts_node, _do_while_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        # Entry is body (id=0), condition (id=1)
        assert frag.entry == 0
        assert frag.exits == frozenset({1})  # condition FALSE exits
        edge_set = {(e.source, e.target, e.kind) for e in edges}
        assert (0, 1, EdgeKind.NORMAL) in edge_set  # body -> condition
        assert (1, 0, EdgeKind.BACK) in edge_set  # condition -> body (TRUE)


class TestHandleSwitch:
    def test_switch_no_fallthrough(self):
        factory = NodeFactory()
        value_node = StubTSNode(type="identifier", text=b"x")
        arm1 = StubTSNode(type="case", text=b"case 1: a;")
        arm2 = StubTSNode(type="case", text=b"case 2: b;")
        body_node = StubTSNode(type="switch_body", named_children=[arm1, arm2])
        ts_node = StubTSNode(
            type="switch_expression",
            _fields={"value": value_node, "body": body_node},
        )

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_switch(
            ts_node,
            _switch_spec(),
            factory,
            build_fn,
            _make_lang_spec(fallthrough=False),
            {},
        )

        assert frag.entry == 0  # value node
        edge_set = {(e.source, e.target, e.kind) for e in edges}
        # value -> arm1, value -> arm2
        assert (0, 1, EdgeKind.NORMAL) in edge_set
        assert (0, 2, EdgeKind.NORMAL) in edge_set
        # No FALLTHROUGH edges
        assert not any(e.kind == EdgeKind.FALLTHROUGH for e in edges)

    def test_switch_with_fallthrough(self):
        factory = NodeFactory()
        value_node = StubTSNode(type="identifier", text=b"x")
        arm1 = StubTSNode(type="case", text=b"case 1: a;")
        arm2 = StubTSNode(type="case", text=b"case 2: b;")
        body_node = StubTSNode(type="switch_body", named_children=[arm1, arm2])
        ts_node = StubTSNode(
            type="switch_expression",
            _fields={"value": value_node, "body": body_node},
        )

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_switch(
            ts_node,
            _switch_spec(),
            factory,
            build_fn,
            _make_lang_spec(fallthrough=True),
            {},
        )

        edge_set = {(e.source, e.target, e.kind) for e in edges}
        # FALLTHROUGH from arm1 -> arm2
        assert (1, 2, EdgeKind.FALLTHROUGH) in edge_set

    def test_switch_with_break_resolves(self):
        factory = NodeFactory()
        value_node = StubTSNode(type="identifier", text=b"x")
        arm1 = StubTSNode(type="case", text=b"case 1: break;")
        body_node = StubTSNode(type="switch_body", named_children=[arm1])
        ts_node = StubTSNode(
            type="switch_expression",
            _fields={"value": value_node, "body": body_node},
        )

        def build_fn(n):
            if n.type == "case":
                node = factory.create(n, ControlFlowRole.BREAK)
                return (
                    Fragment(
                        entry=node.id,
                        exits=frozenset(),
                        pending_breaks=(PendingEdge(source_id=node.id),),
                    ),
                    (),
                )
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, _ = handle_switch(
            ts_node,
            _switch_spec(),
            factory,
            build_fn,
            _make_lang_spec(fallthrough=False),
            {},
        )

        # Break should be resolved
        assert frag.pending_breaks == ()
        assert 1 in frag.exits  # the break node becomes an exit


class TestHandleTry:
    def test_try_catch(self):
        factory = NodeFactory()
        body = StubTSNode(type="block", text=b"{ risky(); }")
        handler = StubTSNode(type="catch_clause", text=b"catch(e) { fix(); }")
        ts_node = StubTSNode(
            type="try_statement",
            _fields={"body": body, "handler": handler},
        )

        def build_fn(n):
            if n.type == "block":
                node = factory.create(n, ControlFlowRole.LEAF)
                return (
                    Fragment(
                        entry=node.id,
                        exits=frozenset({node.id}),
                        pending_throws=(node.id,),
                    ),
                    (),
                )
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_try(
            ts_node, _try_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        assert frag.entry == 0  # body
        edge_set = {(e.source, e.target, e.kind) for e in edges}
        # body throws -> handler
        assert (0, 1, EdgeKind.EXCEPTION) in edge_set

    def test_try_finally(self):
        factory = NodeFactory()
        body = StubTSNode(type="block", text=b"{ a; }")
        finalizer = StubTSNode(type="finally", text=b"finally { cleanup(); }")
        ts_node = StubTSNode(
            type="try_statement",
            _fields={"body": body, "finalizer": finalizer},
        )

        def build_fn(n):
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_try(
            ts_node, _try_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        assert frag.entry == 0  # body
        assert frag.exits == frozenset({1})  # finally exits
        edge_set = {(e.source, e.target, e.kind) for e in edges}
        # body -> finally (NORMAL)
        assert (0, 1, EdgeKind.NORMAL) in edge_set

    def test_try_catch_finally_return_funneled(self):
        factory = NodeFactory()
        body = StubTSNode(type="block", text=b"{ return x; }")
        handler = StubTSNode(type="catch_clause", text=b"catch(e) {}")
        finalizer = StubTSNode(type="finally", text=b"finally { cleanup(); }")
        ts_node = StubTSNode(
            type="try_statement",
            _fields={"body": body, "handler": handler, "finalizer": finalizer},
        )

        def build_fn(n):
            if n.type == "block":
                node = factory.create(n, ControlFlowRole.LEAF)
                return (
                    Fragment(
                        entry=node.id,
                        exits=frozenset(),
                        pending_returns=(node.id,),
                    ),
                    (),
                )
            node = factory.create(n, ControlFlowRole.LEAF)
            return Fragment(entry=node.id, exits=frozenset({node.id})), ()

        frag, edges = handle_try(
            ts_node, _try_spec(), factory, build_fn, _make_lang_spec(), {}
        )

        # Return should be funneled through finally
        edge_set = {(e.source, e.target, e.kind) for e in edges}
        # body return -> finally
        assert (0, 2, EdgeKind.NORMAL) in edge_set
        # Returns are re-pended from finally exits
        assert frag.pending_returns == (0,)
