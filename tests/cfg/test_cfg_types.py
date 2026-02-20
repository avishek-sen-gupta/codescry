"""Unit tests for CFG constructor types."""

import pytest

from repo_surveyor.cfg_constructor.types import (
    VALID_SLOTS,
    ControlFlowRole,
    FieldMapping,
    NodeCFGSpec,
    SemanticSlot,
)


class TestFieldMapping:
    """Tests for the FieldMapping dataclass."""

    def test_default_is_empty(self):
        fm = FieldMapping()
        assert fm.slots == {}

    def test_equality(self):
        fm1 = FieldMapping(slots={"condition": "cond"})
        fm2 = FieldMapping(slots={"condition": "cond"})
        assert fm1 == fm2

    def test_frozen(self):
        fm = FieldMapping()
        with pytest.raises(AttributeError):
            fm.slots = {"x": "y"}


class TestNodeCFGSpec:
    """Tests for the NodeCFGSpec dataclass."""

    def test_default_field_mapping(self):
        spec = NodeCFGSpec(role=ControlFlowRole.BRANCH)
        assert spec.field_mapping == FieldMapping()

    def test_equality(self):
        fm = FieldMapping(slots={"body": "block"})
        s1 = NodeCFGSpec(role=ControlFlowRole.LOOP, field_mapping=fm)
        s2 = NodeCFGSpec(role=ControlFlowRole.LOOP, field_mapping=fm)
        assert s1 == s2


class TestSemanticSlot:
    """Tests for SemanticSlot constants."""

    _EXPECTED_SLOTS = [
        "CONDITION",
        "CONSEQUENCE",
        "ALTERNATIVE",
        "VALUE",
        "BODY",
        "INITIALIZER",
        "UPDATE",
        "HANDLER",
        "FINALIZER",
    ]

    @pytest.mark.parametrize("slot_name", _EXPECTED_SLOTS)
    def test_slot_exists(self, slot_name):
        assert hasattr(SemanticSlot, slot_name)
        assert isinstance(getattr(SemanticSlot, slot_name), str)


class TestValidSlots:
    """Tests for the VALID_SLOTS mapping."""

    _ROLES_WITH_SLOTS = [
        ControlFlowRole.BRANCH,
        ControlFlowRole.SWITCH,
        ControlFlowRole.LOOP,
        ControlFlowRole.LOOP_POST_CONDITION,
        ControlFlowRole.TRY,
    ]

    _ROLES_WITHOUT_SLOTS = [
        ControlFlowRole.SEQUENCE,
        ControlFlowRole.LEAF,
        ControlFlowRole.RETURN,
        ControlFlowRole.BREAK,
        ControlFlowRole.CONTINUE,
        ControlFlowRole.THROW,
        ControlFlowRole.CALL,
    ]

    @pytest.mark.parametrize("role", _ROLES_WITH_SLOTS)
    def test_role_has_slots(self, role):
        assert role in VALID_SLOTS
        assert len(VALID_SLOTS[role]) > 0

    @pytest.mark.parametrize("role", _ROLES_WITHOUT_SLOTS)
    def test_role_has_no_slots(self, role):
        assert role not in VALID_SLOTS

    def test_completeness(self):
        all_roles = set(ControlFlowRole)
        covered = set(self._ROLES_WITH_SLOTS) | set(self._ROLES_WITHOUT_SLOTS)
        assert covered == all_roles
