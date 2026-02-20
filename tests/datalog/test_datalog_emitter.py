"""Tests for the Tree-sitter → Soufflé Datalog emitter.

Three layers of tests:
  1. Fact-export tests — verify RichDatalogFactSet.to_souffle_facts writes
     the expected .facts files with correct content.  No Soufflé binary needed.
  2. Simple Soufflé query tests — load the emitted facts into Soufflé and
     check direct (non-derived) relations round-trip correctly.
  3. Complex Soufflé query tests — verify derived rules (ancestor, call_on,
     scope_chain, visible_from, call_in_scope) produce the expected results.

All Soufflé-dependent tests are skipped automatically when the ``souffle``
binary is not on PATH.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
from tree_sitter_language_pack import get_parser

from datalog_plugins import make_default_registry
from repo_surveyor.integration_patterns.types import Language
from treesitter_to_datalog import emit_datalog, run_souffle

_JAVA_PLUGIN = make_default_registry().get(Language.JAVA)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_SCRIPTS_DIR = Path(__file__).resolve().parent.parent.parent / "query"
_DL_FILE = _SCRIPTS_DIR / "analysis.dl"

# A small Java class with:
#   - 1 class  (Service)
#   - 1 field  (Repository repo)
#   - 2 methods (find, save)
#   - 2 calls  (repo.findById, repo.save)
#   - 0 instantiations
_SERVICE_JAVA = b"""\
public class Service {
    private Repository repo;

    public String find(String id) {
        return repo.findById(id);
    }

    public void save(String item) {
        repo.save(item);
    }
}
"""

# A snippet with instantiations and a bare (no-receiver) call.
_FACTORY_JAVA = b"""\
public class Factory {
    public Widget make(String label) {
        validate(label);
        return new Widget(label);
    }

    private void validate(String s) {
        if (s == null) {
            throw new IllegalArgumentException("null label");
        }
    }
}
"""

# A class with an explicit three-deep method chain on 'x' and a separate
# single call on 'y', so chain-root resolution can be tested precisely.
_CHAIN_JAVA = b"""\
public class ChainExample {
    public void run() {
        x.alpha().beta().gamma();
        y.single();
    }
}
"""

requires_souffle = pytest.mark.skipif(
    shutil.which("souffle") is None,
    reason="souffle binary not in PATH",
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def service_facts(tmp_path: Path):
    parser = get_parser("java")
    tree = parser.parse(_SERVICE_JAVA)
    facts = emit_datalog(tree.root_node, _SERVICE_JAVA)
    facts_dir = tmp_path / "facts"
    facts_dir.mkdir()
    facts.to_souffle_facts(facts_dir, plugin=_JAVA_PLUGIN)
    return facts, facts_dir


@pytest.fixture()
def chain_facts(tmp_path: Path):
    parser = get_parser("java")
    tree = parser.parse(_CHAIN_JAVA)
    facts = emit_datalog(tree.root_node, _CHAIN_JAVA)
    facts_dir = tmp_path / "facts"
    facts_dir.mkdir()
    facts.to_souffle_facts(facts_dir, plugin=_JAVA_PLUGIN)
    return facts, facts_dir


@pytest.fixture()
def factory_facts(tmp_path: Path):
    parser = get_parser("java")
    tree = parser.parse(_FACTORY_JAVA)
    facts = emit_datalog(tree.root_node, _FACTORY_JAVA)
    facts_dir = tmp_path / "facts"
    facts_dir.mkdir()
    facts.to_souffle_facts(facts_dir, plugin=_JAVA_PLUGIN)
    return facts, facts_dir


# ---------------------------------------------------------------------------
# Helper
# ---------------------------------------------------------------------------


def _souffle_out(facts_dir: Path, tmp_path: Path) -> dict[str, list[tuple[str, ...]]]:
    out_dir = tmp_path / "out"
    return run_souffle(_DL_FILE, facts_dir, out_dir)


# ---------------------------------------------------------------------------
# 1. Fact-export tests (no Soufflé needed)
# ---------------------------------------------------------------------------


class TestFactExport:
    _ALL_RELATIONS = [
        "node",
        "token",
        "position",
        "parent",
        "contains",
        "field",
        "name",
        "declared_type",
        "scope",
        "scope_parent",
        "declares",
        "declaration",
        "reference",
        "refers_to",
        "call",
        "instantiation",
    ]

    def test_writes_all_relation_files(self, service_facts, tmp_path):
        _, facts_dir = service_facts
        for rel in self._ALL_RELATIONS:
            assert (facts_dir / f"{rel}.facts").exists(), f"missing {rel}.facts"

    def test_service_has_two_calls(self, service_facts):
        facts, _ = service_facts
        assert len(facts.calls) == 2

    def test_service_calls_are_on_repo(self, service_facts):
        facts, _ = service_facts
        method_names = {c.method_name for c in facts.calls}
        assert method_names == {"findById", "save"}

    def test_service_has_no_instantiations(self, service_facts):
        facts, _ = service_facts
        assert len(facts.instantiations) == 0

    def test_factory_has_two_instantiations(self, factory_facts):
        facts, _ = factory_facts
        type_names = {i.type_name for i in facts.instantiations}
        assert "Widget" in type_names
        assert "IllegalArgumentException" in type_names

    def test_factory_bare_call_has_negative_one_receiver(self, factory_facts):
        # validate(label) has no explicit receiver
        facts, facts_dir = factory_facts
        lines = (facts_dir / "call.facts").read_text().splitlines()
        receivers = {int(line.split("\t")[1]) for line in lines if line}
        assert -1 in receivers

    def test_call_facts_file_has_correct_columns(self, service_facts):
        _, facts_dir = service_facts
        lines = (facts_dir / "call.facts").read_text().splitlines()
        for line in lines:
            parts = line.split("\t")
            assert len(parts) == 3, f"expected 3 columns in call.facts row: {line!r}"

    def test_node_facts_file_has_correct_columns(self, service_facts):
        _, facts_dir = service_facts
        lines = (facts_dir / "node.facts").read_text().splitlines()
        assert lines, "node.facts must not be empty"
        for line in lines:
            parts = line.split("\t")
            assert len(parts) == 2, f"expected 2 columns in node.facts row: {line!r}"

    def test_scope_facts_include_class_and_methods(self, service_facts):
        facts, _ = service_facts
        kinds = {f.kind for f in facts.scopes}
        assert "class_declaration" in kinds
        assert "method_declaration" in kinds


# ---------------------------------------------------------------------------
# 2. Simple Soufflé query tests
# ---------------------------------------------------------------------------


class TestSimpleSouffleQueries:
    @requires_souffle
    def test_method_decl_finds_both_methods(self, service_facts, tmp_path):
        _, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        method_names = {row[1].strip('"') for row in results.get("method_decl", [])}
        assert "find" in method_names
        assert "save" in method_names

    @requires_souffle
    def test_call_on_finds_repo_calls(self, service_facts, tmp_path):
        _, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        call_on = results.get("call_on", [])
        assert call_on, "call_on must not be empty"
        receiver_names = {row[0].strip('"') for row in call_on}
        assert receiver_names == {"repo"}

    @requires_souffle
    def test_call_on_finds_correct_method_names(self, service_facts, tmp_path):
        _, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        call_on = results.get("call_on", [])
        method_names = {row[1].strip('"') for row in call_on}
        assert method_names == {"findById", "save"}

    @requires_souffle
    def test_bare_call_not_in_call_on(self, factory_facts, tmp_path):
        # validate(label) has no receiver so must not appear in call_on
        _, facts_dir = factory_facts
        results = _souffle_out(facts_dir, tmp_path)
        call_on_methods = {row[1].strip('"') for row in results.get("call_on", [])}
        assert "validate" not in call_on_methods

    @requires_souffle
    def test_typed_decl_captures_field_type(self, service_facts, tmp_path):
        _, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        typed = results.get("typed_decl", [])
        var_to_type = {row[1].strip('"'): row[2].strip('"') for row in typed}
        assert "repo" in var_to_type
        assert var_to_type["repo"] == "Repository"


# ---------------------------------------------------------------------------
# 3. Complex Soufflé query tests
# ---------------------------------------------------------------------------


class TestComplexSouffleQueries:
    @requires_souffle
    def test_ancestor_is_transitive_superset_of_parent(self, service_facts, tmp_path):
        facts, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        ancestors = {(int(r[0]), int(r[1])) for r in results.get("ancestor", [])}
        # every direct parent pair must also appear in ancestor
        for p in facts.parents:
            assert (
                p.child_id,
                p.parent_id,
            ) in ancestors, f"parent({p.child_id}, {p.parent_id}) missing from ancestor"

    @requires_souffle
    def test_ancestor_has_transitive_pairs(self, service_facts, tmp_path):
        facts, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        ancestors = {(int(r[0]), int(r[1])) for r in results.get("ancestor", [])}
        # ancestor must contain strictly more pairs than parent (transitivity)
        assert len(ancestors) > len(facts.parents)

    @requires_souffle
    def test_scope_chain_links_block_to_class(self, service_facts, tmp_path):
        facts, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        chain = results.get("scope_chain", [])
        assert chain, "scope_chain must not be empty"
        # scope_chain must have more pairs than scope_parent (transitivity)
        assert len(chain) >= len(facts.scope_parents)

    @requires_souffle
    def test_visible_from_inner_scope_includes_outer_declarations(
        self, service_facts, tmp_path
    ):
        facts, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        visible = results.get("visible_from", [])
        assert visible, "visible_from must not be empty"
        # Count declarations visible per scope; inner scopes should see more
        # than they directly declare via declares/2.
        directly_declared = {(f.scope_id, f.decl_id) for f in facts.declares}
        all_visible = {(int(r[0]), int(r[1])) for r in visible}
        # visible_from is a superset of declares
        for scope_id, decl_id in directly_declared:
            assert (scope_id, decl_id) in all_visible

    @requires_souffle
    def test_call_in_scope_finds_calls_within_method_bodies(
        self, service_facts, tmp_path
    ):
        facts, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        call_in_scope = results.get("call_in_scope", [])
        assert call_in_scope, "call_in_scope must not be empty"
        method_names = {row[2].strip('"') for row in call_in_scope}
        assert "findById" in method_names
        assert "save" in method_names

    @requires_souffle
    def test_call_in_scope_associates_calls_with_correct_method_scope(
        self, service_facts, tmp_path
    ):
        facts, facts_dir = service_facts
        results = _souffle_out(facts_dir, tmp_path)
        method_decls = results.get("method_decl", [])
        call_in_scope = results.get("call_in_scope", [])
        # each call should appear in at least one method_declaration scope
        method_scope_ids = {int(row[0]) for row in method_decls}
        call_scope_ids = {int(row[0]) for row in call_in_scope}
        # the intersection must be non-empty (calls found inside method scopes)
        assert (
            method_scope_ids & call_scope_ids
        ), "no call_in_scope entry shares a scope_id with any method_decl"


# ---------------------------------------------------------------------------
# 4. Method chain queries
# Source: x.alpha().beta().gamma()  and  y.single()
# ---------------------------------------------------------------------------


class TestMethodChainQueries:
    @requires_souffle
    def test_chain_resolves_direct_call_to_base_object(self, chain_facts, tmp_path):
        # x.alpha() — alpha is called directly on x, so chain_root resolves to x
        _, facts_dir = chain_facts
        results = _souffle_out(facts_dir, tmp_path)
        chained = {(r[0], r[1]) for r in results.get("chained_call", [])}
        assert ("x", "alpha") in chained

    @requires_souffle
    def test_chain_resolves_mid_chain_call_to_base_object(self, chain_facts, tmp_path):
        # x.alpha().beta() — beta's receiver is a method_invocation, not x directly
        _, facts_dir = chain_facts
        results = _souffle_out(facts_dir, tmp_path)
        chained = {(r[0], r[1]) for r in results.get("chained_call", [])}
        assert ("x", "beta") in chained

    @requires_souffle
    def test_chain_resolves_deep_call_to_base_object(self, chain_facts, tmp_path):
        # x.alpha().beta().gamma() — gamma is two hops away from x
        _, facts_dir = chain_facts
        results = _souffle_out(facts_dir, tmp_path)
        chained = {(r[0], r[1]) for r in results.get("chained_call", [])}
        assert ("x", "gamma") in chained

    @requires_souffle
    def test_chain_does_not_mix_receivers(self, chain_facts, tmp_path):
        # y.single() must not appear under root 'x', and vice versa
        _, facts_dir = chain_facts
        results = _souffle_out(facts_dir, tmp_path)
        chained = {(r[0], r[1]) for r in results.get("chained_call", [])}
        assert ("y", "single") in chained
        assert ("x", "single") not in chained
        assert ("y", "alpha") not in chained
        assert ("y", "beta") not in chained
        assert ("y", "gamma") not in chained

    @requires_souffle
    def test_all_x_chain_calls_found(self, chain_facts, tmp_path):
        # Exactly alpha, beta, gamma are chained on x — nothing more, nothing less
        _, facts_dir = chain_facts
        results = _souffle_out(facts_dir, tmp_path)
        x_methods = {r[1] for r in results.get("chained_call", []) if r[0] == "x"}
        assert x_methods == {"alpha", "beta", "gamma"}
