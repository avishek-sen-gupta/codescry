"""Tests for the CFG role registry loader."""

import json

import pytest

from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec, load_cfg_roles
from repo_surveyor.cfg_constructor.types import (
    ControlFlowRole,
    FieldMapping,
    NodeCFGSpec,
    SemanticSlot,
)
from repo_surveyor.integration_patterns.types import Language


def _write_lang_config(tmp_path, lang_dir, node_specs):
    """Write a per-language cfg_roles.json under tmp_path/{lang_dir}/."""
    lang_path = tmp_path / lang_dir
    lang_path.mkdir(exist_ok=True)
    cfg_path = lang_path / "cfg_roles.json"
    cfg_path.write_text(json.dumps(node_specs))
    return cfg_path


@pytest.fixture()
def sample_patterns_dir(tmp_path):
    """A minimal patterns dir with java and python cfg_roles.json files."""
    _write_lang_config(
        tmp_path,
        "java",
        {
            "if_statement": "branch",
            "while_statement": "loop",
            "method_invocation": "call",
            "return_statement": "return",
        },
    )
    _write_lang_config(
        tmp_path,
        "python",
        {
            "if_statement": "branch",
            "for_statement": "loop",
            "raise_statement": "throw",
        },
    )
    return tmp_path


class TestLoadCfgRoles:
    """Tests for load_cfg_roles()."""

    def test_loads_only_matching_languages(self, sample_patterns_dir):
        result = load_cfg_roles(sample_patterns_dir)

        assert Language.JAVA in result
        assert Language.PYTHON in result
        assert len(result) == 2

    def test_parses_role_values_correctly(self, sample_patterns_dir):
        result = load_cfg_roles(sample_patterns_dir)

        java_spec = result[Language.JAVA]
        assert java_spec.node_specs["if_statement"].role == ControlFlowRole.BRANCH
        assert java_spec.node_specs["while_statement"].role == ControlFlowRole.LOOP
        assert java_spec.node_specs["method_invocation"].role == ControlFlowRole.CALL
        assert java_spec.node_specs["return_statement"].role == ControlFlowRole.RETURN

    def test_unmapped_node_returns_leaf(self, sample_patterns_dir):
        result = load_cfg_roles(sample_patterns_dir)

        java_spec = result[Language.JAVA]
        assert java_spec.role_for("nonexistent_node") == ControlFlowRole.LEAF

    def test_unknown_role_value_maps_to_leaf(self, tmp_path):
        _write_lang_config(tmp_path, "java", {"weird_node": "totally_invalid_role"})
        result = load_cfg_roles(tmp_path)

        assert (
            result[Language.JAVA].node_specs["weird_node"].role == ControlFlowRole.LEAF
        )

    def test_empty_patterns_dir_returns_empty_dict(self, tmp_path):
        result = load_cfg_roles(tmp_path)

        assert result == {}

    def test_missing_dir_returns_empty_dict(self, tmp_path):
        result = load_cfg_roles(tmp_path / "nonexistent")

        assert result == {}

    def test_language_has_correct_language_field(self, sample_patterns_dir):
        result = load_cfg_roles(sample_patterns_dir)

        assert result[Language.JAVA].language == Language.JAVA
        assert result[Language.PYTHON].language == Language.PYTHON

    def test_csharp_directory_mapping(self, tmp_path):
        _write_lang_config(tmp_path, "csharp", {"if_statement": "branch"})
        result = load_cfg_roles(tmp_path)

        assert Language.CSHARP in result

    def test_cpp_directory_mapping(self, tmp_path):
        _write_lang_config(tmp_path, "cpp", {"if_statement": "branch"})
        result = load_cfg_roles(tmp_path)

        assert Language.CPP in result


class TestGetCfgSpec:
    """Tests for get_cfg_spec()."""

    def test_known_language_returns_spec(self, sample_patterns_dir):
        spec = get_cfg_spec(Language.JAVA, sample_patterns_dir)

        assert spec.language == Language.JAVA
        assert spec.node_specs["if_statement"].role == ControlFlowRole.BRANCH
        assert len(spec.node_specs) == 4

    def test_unknown_language_returns_empty_null_object_spec(self, sample_patterns_dir):
        spec = get_cfg_spec(Language.KOTLIN, sample_patterns_dir)

        assert spec.language == Language.KOTLIN
        assert spec.node_specs == {}
        assert spec.role_for("anything") == ControlFlowRole.LEAF

    def test_missing_dir_returns_empty_spec(self, tmp_path):
        spec = get_cfg_spec(Language.JAVA, tmp_path / "nonexistent")

        assert spec.language == Language.JAVA
        assert spec.node_specs == {}


class TestExtendedNodeSpec:
    """Tests for the extended object form in cfg_roles.json."""

    def test_extended_spec_parses_role_and_fields(self, tmp_path):
        _write_lang_config(
            tmp_path,
            "java",
            {
                "if_statement": {
                    "role": "branch",
                    "condition": "condition",
                    "consequence": "consequence",
                    "alternative": "alternative",
                }
            },
        )
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        node = spec.node_specs["if_statement"]
        assert node.role == ControlFlowRole.BRANCH
        assert node.field_mapping.slots == {
            "condition": "condition",
            "consequence": "consequence",
            "alternative": "alternative",
        }

    def test_extended_spec_with_positional_int(self, tmp_path):
        _write_lang_config(
            tmp_path,
            "java",
            {"if_statement": {"role": "branch", "condition": 0, "consequence": 1}},
        )
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        node = spec.node_specs["if_statement"]
        assert node.field_mapping.slots == {"condition": 0, "consequence": 1}

    def test_mixed_simple_and_extended(self, tmp_path):
        _write_lang_config(
            tmp_path,
            "java",
            {
                "break_statement": "break",
                "if_statement": {
                    "role": "branch",
                    "condition": "condition",
                    "consequence": "consequence",
                },
            },
        )
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        assert spec.node_specs["break_statement"].role == ControlFlowRole.BREAK
        assert spec.node_specs["break_statement"].field_mapping.slots == {}
        assert spec.node_specs["if_statement"].role == ControlFlowRole.BRANCH
        assert "condition" in spec.node_specs["if_statement"].field_mapping.slots

    def test_invalid_slots_silently_dropped(self, tmp_path):
        _write_lang_config(
            tmp_path,
            "java",
            {
                "if_statement": {
                    "role": "branch",
                    "condition": "condition",
                    "body": "body_field",
                }
            },
        )
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        node = spec.node_specs["if_statement"]
        assert "condition" in node.field_mapping.slots
        assert "body" not in node.field_mapping.slots

    def test_missing_role_key_defaults_to_leaf(self, tmp_path):
        _write_lang_config(tmp_path, "java", {"some_node": {"condition": "cond_field"}})
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        assert spec.node_specs["some_node"].role == ControlFlowRole.LEAF
        assert spec.node_specs["some_node"].field_mapping.slots == {}

    def test_role_for_with_extended_specs(self, tmp_path):
        _write_lang_config(
            tmp_path,
            "java",
            {
                "if_statement": {"role": "branch", "condition": "condition"},
                "break_statement": "break",
            },
        )
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        assert spec.role_for("if_statement") == ControlFlowRole.BRANCH
        assert spec.role_for("break_statement") == ControlFlowRole.BREAK
        assert spec.role_for("nonexistent") == ControlFlowRole.LEAF


class TestSpecFor:
    """Tests for LanguageCFGSpec.spec_for()."""

    def test_mapped_node_returns_full_spec(self, tmp_path):
        _write_lang_config(
            tmp_path,
            "java",
            {
                "if_statement": {
                    "role": "branch",
                    "condition": "condition",
                    "consequence": "consequence",
                }
            },
        )
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        node = spec.spec_for("if_statement")
        assert node.role == ControlFlowRole.BRANCH
        assert node.field_mapping.slots == {
            "condition": "condition",
            "consequence": "consequence",
        }

    def test_unmapped_node_returns_null_object(self, tmp_path):
        _write_lang_config(tmp_path, "java", {"if_statement": "branch"})
        spec = get_cfg_spec(Language.JAVA, tmp_path)

        node = spec.spec_for("nonexistent_node")
        assert node.role == ControlFlowRole.LEAF
        assert node.field_mapping.slots == {}
