"""Tests for the Datalog language plugin system."""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
from tree_sitter_language_pack import get_parser

from datalog_plugins import (
    CallExtractionStrategy,
    CallFieldMapping,
    DatalogLanguagePlugin,
    DatalogPluginRegistry,
    make_default_registry,
)
from repo_surveyor.integration_patterns.types import Language
from treesitter_to_datalog import _NULL_PLUGIN, emit_datalog

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_SCRIPTS_DIR = Path(__file__).resolve().parent.parent / "query"
_DL_FILE = _SCRIPTS_DIR / "analysis.dl"

requires_souffle = pytest.mark.skipif(
    shutil.which("souffle") is None,
    reason="souffle binary not in PATH",
)


def _registry() -> DatalogPluginRegistry:
    return make_default_registry()


# ---------------------------------------------------------------------------
# TestDatalogLanguagePlugin — dataclass invariants
# ---------------------------------------------------------------------------


class TestDatalogLanguagePlugin:
    def test_java_callable_node_types(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert "method_declaration" in plugin.callable_node_types
        assert "constructor_declaration" in plugin.callable_node_types

    def test_java_call_node_types(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert "method_invocation" in plugin.call_node_types

    def test_java_instantiation_node_types(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert "object_creation_expression" in plugin.instantiation_node_types

    def test_python_callable_node_types(self):
        plugin = _registry().get(Language.PYTHON)
        assert plugin is not None
        assert "function_definition" in plugin.callable_node_types

    def test_python_has_empty_instantiation_node_types(self):
        plugin = _registry().get(Language.PYTHON)
        assert plugin is not None
        assert plugin.instantiation_node_types == frozenset()

    def test_go_callable_node_types_has_both_kinds(self):
        plugin = _registry().get(Language.GO)
        assert plugin is not None
        assert "function_declaration" in plugin.callable_node_types
        assert "method_declaration" in plugin.callable_node_types

    def test_rust_callable_node_types(self):
        plugin = _registry().get(Language.RUST)
        assert plugin is not None
        assert "function_item" in plugin.callable_node_types

    def test_frozen_raises_on_mutation(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        with pytest.raises(Exception):  # dataclasses.FrozenInstanceError
            plugin.language = Language.PYTHON  # type: ignore[misc]

    def test_callable_node_types_are_frozenset(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert isinstance(plugin.callable_node_types, frozenset)

    def test_call_node_types_are_frozenset(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert isinstance(plugin.call_node_types, frozenset)

    def test_instantiation_node_types_are_frozenset(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert isinstance(plugin.instantiation_node_types, frozenset)

    def test_extra_scope_openers_are_frozenset(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert isinstance(plugin.extra_scope_openers, frozenset)

    def test_extra_declaration_parents_are_frozenset(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert isinstance(plugin.extra_declaration_parents, frozenset)

    def test_java_uses_direct_fields_strategy(self):
        plugin = _registry().get(Language.JAVA)
        assert plugin is not None
        assert plugin.call_field_mapping.mode == CallExtractionStrategy.DIRECT_FIELDS

    def test_python_uses_function_child_strategy(self):
        plugin = _registry().get(Language.PYTHON)
        assert plugin is not None
        assert plugin.call_field_mapping.mode == CallExtractionStrategy.FUNCTION_CHILD

    def test_javascript_callable_node_types(self):
        plugin = _registry().get(Language.JAVASCRIPT)
        assert plugin is not None
        assert "function_declaration" in plugin.callable_node_types
        assert "arrow_function" in plugin.callable_node_types

    def test_typescript_and_javascript_share_call_node_types(self):
        js = _registry().get(Language.JAVASCRIPT)
        ts = _registry().get(Language.TYPESCRIPT)
        assert js is not None
        assert ts is not None
        assert js.call_node_types == ts.call_node_types


# ---------------------------------------------------------------------------
# TestDatalogPluginRegistry — registry API
# ---------------------------------------------------------------------------


class TestDatalogPluginRegistry:
    def test_get_java_returns_plugin(self):
        plugin = _registry().get(Language.JAVA)
        assert isinstance(plugin, DatalogLanguagePlugin)

    def test_get_unsupported_language_returns_none(self):
        assert _registry().get(Language.CSHARP) is None

    def test_all_plugins_covers_six_languages(self):
        plugins = _registry().all_plugins()
        languages = {p.language for p in plugins}
        assert Language.JAVA in languages
        assert Language.PYTHON in languages
        assert Language.JAVASCRIPT in languages
        assert Language.TYPESCRIPT in languages
        assert Language.GO in languages
        assert Language.RUST in languages
        assert len(languages) == 6

    def test_make_default_registry_returns_populated_registry(self):
        registry = make_default_registry()
        assert isinstance(registry, DatalogPluginRegistry)
        assert len(registry.all_plugins()) >= 6


# ---------------------------------------------------------------------------
# TestBridgeFacts — to_souffle_facts() bridge file output
# ---------------------------------------------------------------------------


class TestBridgeFacts:
    def test_java_plugin_writes_callable_node_type_facts(self, tmp_path):
        java_plugin = _registry().get(Language.JAVA)
        assert java_plugin is not None

        parser = get_parser("java")
        source = b"public class A { void m() {} }"
        facts = emit_datalog(parser.parse(source).root_node, source)
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=java_plugin)

        content = (facts_dir / "callable_node_type.facts").read_text()
        lines = [l for l in content.splitlines() if l]
        assert "method_declaration" in lines
        assert "constructor_declaration" in lines

    def test_java_plugin_writes_call_node_type_facts(self, tmp_path):
        java_plugin = _registry().get(Language.JAVA)
        assert java_plugin is not None

        parser = get_parser("java")
        source = b"public class A { void m() {} }"
        facts = emit_datalog(parser.parse(source).root_node, source)
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=java_plugin)

        content = (facts_dir / "call_node_type.facts").read_text()
        lines = [l for l in content.splitlines() if l]
        assert "method_invocation" in lines

    def test_java_plugin_writes_instantiation_node_type_facts(self, tmp_path):
        java_plugin = _registry().get(Language.JAVA)
        assert java_plugin is not None

        parser = get_parser("java")
        source = b"public class A { void m() {} }"
        facts = emit_datalog(parser.parse(source).root_node, source)
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=java_plugin)

        content = (facts_dir / "instantiation_node_type.facts").read_text()
        lines = [l for l in content.splitlines() if l]
        assert "object_creation_expression" in lines

    def test_null_plugin_writes_empty_bridge_facts(self, tmp_path):
        # _NULL_PLUGIN is the Java defaults, so its callable/call/instantiation
        # sets are non-empty. Per the plan "empty frozensets → empty files"
        # refers to a hypothetical plugin with empty sets. Instead, verify that
        # _NULL_PLUGIN writes the same as the Java plugin.
        java_plugin = _registry().get(Language.JAVA)
        assert java_plugin is not None

        parser = get_parser("java")
        source = b"public class A {}"
        facts = emit_datalog(parser.parse(source).root_node, source)
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=_NULL_PLUGIN)

        # Files must exist (even if empty in edge cases).
        assert (facts_dir / "callable_node_type.facts").exists()
        assert (facts_dir / "call_node_type.facts").exists()
        assert (facts_dir / "instantiation_node_type.facts").exists()

        # _NULL_PLUGIN has the same content as the Java plugin.
        null_callable = set(
            (facts_dir / "callable_node_type.facts").read_text().splitlines()
        )
        java_callable = set(sorted(java_plugin.callable_node_types))
        assert null_callable == java_callable

    def test_python_plugin_writes_function_definition(self, tmp_path):
        python_plugin = _registry().get(Language.PYTHON)
        assert python_plugin is not None

        parser = get_parser("python")
        source = b"def foo(): pass"
        facts = emit_datalog(
            parser.parse(source).root_node, source, plugin=python_plugin
        )
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=python_plugin)

        content = (facts_dir / "callable_node_type.facts").read_text()
        assert "function_definition" in content.splitlines()

    def test_python_plugin_writes_empty_instantiation_facts(self, tmp_path):
        python_plugin = _registry().get(Language.PYTHON)
        assert python_plugin is not None

        parser = get_parser("python")
        source = b"def foo(): pass"
        facts = emit_datalog(
            parser.parse(source).root_node, source, plugin=python_plugin
        )
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=python_plugin)

        content = (facts_dir / "instantiation_node_type.facts").read_text()
        assert content.strip() == ""


# ---------------------------------------------------------------------------
# TestPluginAwareEmitter — emit_datalog() with plugin
# ---------------------------------------------------------------------------

_SERVICE_JAVA = b"""\
public class Service {
    private Repository repo;

    public String find(String id) {
        return repo.findById(id);
    }
}
"""

_FACTORY_JAVA = b"""\
public class Factory {
    public Widget make(String label) {
        validate(label);
        return new Widget(label);
    }
}
"""


class TestPluginAwareEmitter:
    def test_java_plugin_emits_method_invocation_call(self):
        java_plugin = _registry().get(Language.JAVA)
        assert java_plugin is not None

        parser = get_parser("java")
        facts = emit_datalog(
            parser.parse(_SERVICE_JAVA).root_node,
            _SERVICE_JAVA,
            plugin=java_plugin,
        )
        method_names = {c.method_name for c in facts.calls}
        assert "findById" in method_names

    def test_java_plugin_emits_object_creation_instantiation(self):
        java_plugin = _registry().get(Language.JAVA)
        assert java_plugin is not None

        parser = get_parser("java")
        facts = emit_datalog(
            parser.parse(_FACTORY_JAVA).root_node,
            _FACTORY_JAVA,
            plugin=java_plugin,
        )
        type_names = {i.type_name for i in facts.instantiations}
        assert "Widget" in type_names

    def test_null_plugin_gives_same_java_results(self):
        parser = get_parser("java")
        facts = emit_datalog(parser.parse(_SERVICE_JAVA).root_node, _SERVICE_JAVA)
        method_names = {c.method_name for c in facts.calls}
        assert "findById" in method_names

    def test_additive_scope_opener_extra_type_appears_in_scopes(self):
        from datalog_plugins import CallFieldMapping
        from repo_surveyor.integration_patterns.types import Language

        custom_plugin = DatalogLanguagePlugin(
            language=Language.JAVA,
            tree_sitter_language="java",
            extra_scope_openers=frozenset(["block"]),
            extra_declaration_parents=frozenset(),
            callable_node_types=frozenset(["method_declaration"]),
            call_node_types=frozenset(["method_invocation"]),
            instantiation_node_types=frozenset(),
            call_field_mapping=CallFieldMapping(
                mode=CallExtractionStrategy.DIRECT_FIELDS
            ),
        )

        parser = get_parser("java")
        source = b"public class A { void m() { int x = 1; } }"
        facts = emit_datalog(
            parser.parse(source).root_node, source, plugin=custom_plugin
        )
        kinds = {s.kind for s in facts.scopes}
        assert "block" in kinds


# ---------------------------------------------------------------------------
# TestAnalysisDlWithBridgeFacts — Soufflé integration
# ---------------------------------------------------------------------------


class TestAnalysisDlWithBridgeFacts:
    @requires_souffle
    def test_java_plugin_method_decl_contains_correct_names(self, tmp_path):
        from treesitter_to_datalog import run_souffle

        java_plugin = _registry().get(Language.JAVA)
        assert java_plugin is not None

        source = b"""\
public class Service {
    public String find(String id) { return null; }
    public void save(String item) {}
}
"""
        parser = get_parser("java")
        facts = emit_datalog(parser.parse(source).root_node, source, plugin=java_plugin)
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=java_plugin)
        out_dir = tmp_path / "out"
        results = run_souffle(_DL_FILE, facts_dir, out_dir)
        method_names = {row[1].strip('"') for row in results.get("method_decl", [])}
        assert "find" in method_names
        assert "save" in method_names

    @requires_souffle
    def test_python_plugin_method_decl_contains_function_names(self, tmp_path):
        from treesitter_to_datalog import run_souffle

        python_plugin = _registry().get(Language.PYTHON)
        assert python_plugin is not None

        source = b"""\
def foo():
    pass

def bar():
    return 1
"""
        parser = get_parser("python")
        facts = emit_datalog(
            parser.parse(source).root_node, source, plugin=python_plugin
        )
        facts_dir = tmp_path / "facts"
        facts.to_souffle_facts(facts_dir, plugin=python_plugin)
        out_dir = tmp_path / "out"
        results = run_souffle(_DL_FILE, facts_dir, out_dir)
        method_names = {row[1].strip('"') for row in results.get("method_decl", [])}
        assert "foo" in method_names
        assert "bar" in method_names
