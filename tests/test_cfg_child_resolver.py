"""Tests for child_resolver using stub TSNode implementations."""

from dataclasses import dataclass, field

import pytest

from repo_surveyor.cfg_constructor.child_resolver import (
    resolve_body_children,
    resolve_semantic_slot,
    resolve_slot,
)
from repo_surveyor.cfg_constructor.types import (
    ControlFlowRole,
    FieldMapping,
    NodeCFGSpec,
    SemanticSlot,
)


@dataclass
class StubPoint:
    row: int = 0
    column: int = 0


@dataclass
class StubTSNode:
    """Lightweight stub implementing the TSNode protocol."""

    type: str = "stub"
    children: list["StubTSNode"] = field(default_factory=list)
    named_children: list["StubTSNode"] = field(default_factory=list)
    start_point: StubPoint = field(default_factory=StubPoint)
    end_point: StubPoint = field(default_factory=StubPoint)
    text: bytes = b""
    _fields: dict[str, "StubTSNode"] = field(default_factory=dict)

    def child_by_field_name(self, name: str) -> "StubTSNode | None":
        return self._fields.get(name)


class TestResolveSlot:
    """Tests for resolve_slot()."""

    def test_string_ref_found(self):
        child = StubTSNode(type="condition")
        parent = StubTSNode(type="if_statement", _fields={"condition": child})

        result = resolve_slot(parent, "condition")

        assert result == [child]

    def test_string_ref_not_found(self):
        parent = StubTSNode(type="if_statement", _fields={})

        result = resolve_slot(parent, "condition")

        assert result == []

    def test_int_ref_valid_index(self):
        child0 = StubTSNode(type="first")
        child1 = StubTSNode(type="second")
        parent = StubTSNode(type="node", named_children=[child0, child1])

        assert resolve_slot(parent, 0) == [child0]
        assert resolve_slot(parent, 1) == [child1]

    def test_int_ref_out_of_bounds(self):
        parent = StubTSNode(type="node", named_children=[StubTSNode()])

        assert resolve_slot(parent, 5) == []

    def test_int_ref_negative_returns_empty(self):
        parent = StubTSNode(type="node", named_children=[StubTSNode()])

        assert resolve_slot(parent, -1) == []

    def test_int_ref_empty_children(self):
        parent = StubTSNode(type="node", named_children=[])

        assert resolve_slot(parent, 0) == []


class TestResolveSemanticSlot:
    """Tests for resolve_semantic_slot()."""

    def test_resolves_mapped_slot(self):
        cond_node = StubTSNode(type="condition_expr")
        parent = StubTSNode(type="if_statement", _fields={"condition": cond_node})
        spec = NodeCFGSpec(
            role=ControlFlowRole.BRANCH,
            field_mapping=FieldMapping(
                slots={
                    SemanticSlot.CONDITION: "condition",
                    SemanticSlot.CONSEQUENCE: "consequence",
                }
            ),
        )

        result = resolve_semantic_slot(parent, spec, SemanticSlot.CONDITION)

        assert result is cond_node

    def test_returns_none_for_unmapped_slot(self):
        parent = StubTSNode(type="if_statement")
        spec = NodeCFGSpec(
            role=ControlFlowRole.BRANCH,
            field_mapping=FieldMapping(slots={SemanticSlot.CONDITION: "condition"}),
        )

        result = resolve_semantic_slot(parent, spec, SemanticSlot.ALTERNATIVE)

        assert result is None

    def test_returns_none_when_field_missing_on_node(self):
        parent = StubTSNode(type="if_statement", _fields={})
        spec = NodeCFGSpec(
            role=ControlFlowRole.BRANCH,
            field_mapping=FieldMapping(slots={SemanticSlot.CONDITION: "condition"}),
        )

        result = resolve_semantic_slot(parent, spec, SemanticSlot.CONDITION)

        assert result is None

    def test_positional_int_slot(self):
        child = StubTSNode(type="expr")
        parent = StubTSNode(type="node", named_children=[child])
        spec = NodeCFGSpec(
            role=ControlFlowRole.BRANCH,
            field_mapping=FieldMapping(slots={SemanticSlot.CONDITION: 0}),
        )

        result = resolve_semantic_slot(parent, spec, SemanticSlot.CONDITION)

        assert result is child


class TestResolveBodyChildren:
    """Tests for resolve_body_children()."""

    def test_no_body_slot_returns_named_children(self):
        child_a = StubTSNode(type="stmt_a")
        child_b = StubTSNode(type="stmt_b")
        parent = StubTSNode(type="block", named_children=[child_a, child_b])
        spec = NodeCFGSpec(role=ControlFlowRole.SEQUENCE)

        result = resolve_body_children(parent, spec)

        assert result == [child_a, child_b]

    def test_body_slot_resolves_to_body_named_children(self):
        stmt1 = StubTSNode(type="stmt1")
        stmt2 = StubTSNode(type="stmt2")
        body = StubTSNode(type="block", named_children=[stmt1, stmt2])
        parent = StubTSNode(
            type="while_statement",
            _fields={"body": body},
            named_children=[StubTSNode(type="condition"), body],
        )
        spec = NodeCFGSpec(
            role=ControlFlowRole.LOOP,
            field_mapping=FieldMapping(
                slots={SemanticSlot.BODY: "body", SemanticSlot.CONDITION: "condition"}
            ),
        )

        result = resolve_body_children(parent, spec)

        assert result == [stmt1, stmt2]

    def test_body_slot_missing_on_node_returns_empty(self):
        parent = StubTSNode(type="while_statement", _fields={})
        spec = NodeCFGSpec(
            role=ControlFlowRole.LOOP,
            field_mapping=FieldMapping(slots={SemanticSlot.BODY: "body"}),
        )

        result = resolve_body_children(parent, spec)

        assert result == []

    def test_body_slot_with_positional_ref(self):
        stmt = StubTSNode(type="stmt")
        body = StubTSNode(type="block", named_children=[stmt])
        parent = StubTSNode(type="node", named_children=[body])
        spec = NodeCFGSpec(
            role=ControlFlowRole.LOOP,
            field_mapping=FieldMapping(slots={SemanticSlot.BODY: 0}),
        )

        result = resolve_body_children(parent, spec)

        assert result == [stmt]
