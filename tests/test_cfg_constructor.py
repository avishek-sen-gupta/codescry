"""Tests for the CFG constructor role schema and supporting types."""

import pytest

from repo_surveyor.cfg_constructor import ControlFlowRole, LanguageCFGSpec
from repo_surveyor.integration_patterns.types import Language

EXPECTED_ROLES = {
    "SEQUENCE",
    "BRANCH",
    "SWITCH",
    "LOOP",
    "LOOP_POST_CONDITION",
    "RETURN",
    "BREAK",
    "CONTINUE",
    "THROW",
    "TRY",
    "CALL",
    "LEAF",
}


class TestControlFlowRole:
    def test_has_exactly_12_members(self):
        assert len(ControlFlowRole) == 12

    def test_member_names_match_expected(self):
        assert {role.name for role in ControlFlowRole} == EXPECTED_ROLES

    def test_all_values_are_lowercase_strings(self):
        assert all(role.value == role.name.lower() for role in ControlFlowRole)


class TestLanguageCFGSpec:
    @pytest.fixture()
    def java_spec(self) -> LanguageCFGSpec:
        return LanguageCFGSpec(
            language=Language.JAVA,
            node_specs={
                "if_statement": ControlFlowRole.BRANCH,
                "while_statement": ControlFlowRole.LOOP,
                "block": ControlFlowRole.SEQUENCE,
                "return_statement": ControlFlowRole.RETURN,
            },
        )

    def test_construction(self, java_spec: LanguageCFGSpec):
        assert java_spec.language == Language.JAVA
        assert len(java_spec.node_specs) == 4

    def test_frozen(self, java_spec: LanguageCFGSpec):
        with pytest.raises(AttributeError):
            java_spec.language = Language.PYTHON  # type: ignore[misc]

    def test_role_for_mapped_type(self, java_spec: LanguageCFGSpec):
        assert java_spec.role_for("if_statement") == ControlFlowRole.BRANCH
        assert java_spec.role_for("while_statement") == ControlFlowRole.LOOP

    def test_role_for_unmapped_type_returns_leaf(self, java_spec: LanguageCFGSpec):
        assert java_spec.role_for("assignment_expression") == ControlFlowRole.LEAF
        assert java_spec.role_for("nonexistent_node") == ControlFlowRole.LEAF

    def test_empty_spec_always_returns_leaf(self):
        spec = LanguageCFGSpec(language=Language.PYTHON, node_specs={})
        assert spec.role_for("any_node") == ControlFlowRole.LEAF
