"""Tests for the CFG role registry loader."""

import json

import pytest

from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec, load_cfg_roles
from repo_surveyor.cfg_constructor.types import ControlFlowRole
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
        assert java_spec.node_specs["if_statement"] == ControlFlowRole.BRANCH
        assert java_spec.node_specs["while_statement"] == ControlFlowRole.LOOP
        assert java_spec.node_specs["method_invocation"] == ControlFlowRole.CALL
        assert java_spec.node_specs["return_statement"] == ControlFlowRole.RETURN

    def test_unmapped_node_returns_leaf(self, sample_patterns_dir):
        result = load_cfg_roles(sample_patterns_dir)

        java_spec = result[Language.JAVA]
        assert java_spec.role_for("nonexistent_node") == ControlFlowRole.LEAF

    def test_unknown_role_value_maps_to_leaf(self, tmp_path):
        _write_lang_config(tmp_path, "java", {"weird_node": "totally_invalid_role"})
        result = load_cfg_roles(tmp_path)

        assert result[Language.JAVA].node_specs["weird_node"] == ControlFlowRole.LEAF

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
        assert spec.node_specs["if_statement"] == ControlFlowRole.BRANCH
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
