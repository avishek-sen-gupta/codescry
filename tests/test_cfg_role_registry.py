"""Tests for the CFG role registry loader."""

import json

import pytest

from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec, load_cfg_roles
from repo_surveyor.cfg_constructor.types import ControlFlowRole
from repo_surveyor.integration_patterns.types import Language


def _write_config(tmp_path, config):
    """Write a config dict to a temporary cfg_roles.json."""
    path = tmp_path / "cfg_roles.json"
    path.write_text(json.dumps(config))
    return path


@pytest.fixture()
def sample_config():
    """A minimal config with two known languages and one unknown."""
    return {
        "meta": {"generated_at": "2026-01-01T00:00:00Z", "source": "test"},
        "languages": {
            "java": {
                "node_specs": {
                    "if_statement": "branch",
                    "while_statement": "loop",
                    "method_invocation": "call",
                    "return_statement": "return",
                }
            },
            "python": {
                "node_specs": {
                    "if_statement": "branch",
                    "for_statement": "loop",
                    "raise_statement": "throw",
                }
            },
            "zzz_unknown_lang": {"node_specs": {"some_node": "branch"}},
        },
        "failures": {"broken_lang": "some error"},
    }


class TestLoadCfgRoles:
    """Tests for load_cfg_roles()."""

    def test_loads_only_matching_languages(self, tmp_path, sample_config):
        path = _write_config(tmp_path, sample_config)
        result = load_cfg_roles(path)

        assert Language.JAVA in result
        assert Language.PYTHON in result
        assert len(result) == 2

    def test_parses_role_values_correctly(self, tmp_path, sample_config):
        path = _write_config(tmp_path, sample_config)
        result = load_cfg_roles(path)

        java_spec = result[Language.JAVA]
        assert java_spec.node_specs["if_statement"] == ControlFlowRole.BRANCH
        assert java_spec.node_specs["while_statement"] == ControlFlowRole.LOOP
        assert java_spec.node_specs["method_invocation"] == ControlFlowRole.CALL
        assert java_spec.node_specs["return_statement"] == ControlFlowRole.RETURN

    def test_unmapped_node_returns_leaf(self, tmp_path, sample_config):
        path = _write_config(tmp_path, sample_config)
        result = load_cfg_roles(path)

        java_spec = result[Language.JAVA]
        assert java_spec.role_for("nonexistent_node") == ControlFlowRole.LEAF

    def test_unknown_role_value_maps_to_leaf(self, tmp_path):
        config = {
            "meta": {},
            "languages": {
                "java": {"node_specs": {"weird_node": "totally_invalid_role"}}
            },
        }
        path = _write_config(tmp_path, config)
        result = load_cfg_roles(path)

        assert result[Language.JAVA].node_specs["weird_node"] == ControlFlowRole.LEAF

    def test_empty_config_returns_empty_dict(self, tmp_path):
        config = {"meta": {}, "languages": {}}
        path = _write_config(tmp_path, config)
        result = load_cfg_roles(path)

        assert result == {}

    def test_missing_file_returns_empty_dict(self, tmp_path):
        path = tmp_path / "nonexistent.json"
        result = load_cfg_roles(path)

        assert result == {}

    def test_language_has_correct_language_field(self, tmp_path, sample_config):
        path = _write_config(tmp_path, sample_config)
        result = load_cfg_roles(path)

        assert result[Language.JAVA].language == Language.JAVA
        assert result[Language.PYTHON].language == Language.PYTHON

    def test_c_sharp_override_mapping(self, tmp_path):
        config = {
            "meta": {},
            "languages": {"c_sharp": {"node_specs": {"if_statement": "branch"}}},
        }
        path = _write_config(tmp_path, config)
        result = load_cfg_roles(path)

        assert Language.CSHARP in result

    def test_cpp_override_mapping(self, tmp_path):
        config = {
            "meta": {},
            "languages": {"cpp": {"node_specs": {"if_statement": "branch"}}},
        }
        path = _write_config(tmp_path, config)
        result = load_cfg_roles(path)

        assert Language.CPP in result


class TestGetCfgSpec:
    """Tests for get_cfg_spec()."""

    def test_known_language_returns_spec(self, tmp_path, sample_config):
        path = _write_config(tmp_path, sample_config)
        spec = get_cfg_spec(Language.JAVA, path)

        assert spec.language == Language.JAVA
        assert spec.node_specs["if_statement"] == ControlFlowRole.BRANCH
        assert len(spec.node_specs) == 4

    def test_unknown_language_returns_empty_null_object_spec(
        self, tmp_path, sample_config
    ):
        path = _write_config(tmp_path, sample_config)
        spec = get_cfg_spec(Language.KOTLIN, path)

        assert spec.language == Language.KOTLIN
        assert spec.node_specs == {}
        assert spec.role_for("anything") == ControlFlowRole.LEAF

    def test_missing_file_returns_empty_spec(self, tmp_path):
        path = tmp_path / "nonexistent.json"
        spec = get_cfg_spec(Language.JAVA, path)

        assert spec.language == Language.JAVA
        assert spec.node_specs == {}
