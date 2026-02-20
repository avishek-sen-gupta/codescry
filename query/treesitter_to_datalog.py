"""Emit a rich Datalog ontology from a Tree-sitter parse tree.

Relations
─────────
Raw structure (kept for low-level ancestor / positional queries):
  node(id, type)
  token(id, text)
  position(id, start_row, start_col, end_row, end_col)
  parent(child_id, parent_id)

Semantic structure (punctuation nodes filtered out via node.is_named):
  contains(parent_id, child_id)
  field(parent_id, field_name, child_id)

Identity:
  name(node_id, text)          — identifier and type_identifier leaf text
  declared_type(node_id, text) — text of the syntactic type annotation on a node

Scoping:
  scope(scope_id, kind)                     — nodes that open a lexical scope
  scope_parent(inner_scope_id, outer_scope_id) — scope chain
  declares(scope_id, decl_id)               — declaration visible in scope

Use–def (approximate: same-name, innermost-enclosing-scope):
  declaration(node_id)         — identifier that names something being declared
  reference(node_id)           — identifier that refers to something
  refers_to(ref_id, decl_id)   — approximate name resolution

Higher-level:
  call(site_id, receiver_id, method_name)  — method invocation
  instantiation(site_id, type_name)        — object creation expression

Usage:
    poetry run python query/treesitter_to_datalog.py
"""

from __future__ import annotations

import subprocess
import textwrap
from collections import defaultdict
from dataclasses import dataclass, field
from itertools import count
from pathlib import Path
from typing import Iterator

from tree_sitter import Node
from tree_sitter_language_pack import get_parser

# ---------------------------------------------------------------------------
# Node type configuration (language-agnostic defaults)
# ---------------------------------------------------------------------------

_SCOPE_OPENERS: frozenset[str] = frozenset(
    [
        # top-level compilation units
        "program",
        "source_file",
        "compilation_unit",
        "translation_unit",
        # types
        "class_declaration",
        "interface_declaration",
        "enum_declaration",
        "record_declaration",
        "annotation_type_declaration",
        # callables
        "method_declaration",
        "constructor_declaration",
        "function_declaration",
        "function_definition",
        "arrow_function",
        "lambda_expression",
        # blocks
        "class_body",
        "block",
        "constructor_body",
        # control flow with own scope
        "for_statement",
        "enhanced_for_statement",
        "while_statement",
        "do_statement",
        "try_statement",
        "catch_clause",
        "switch_block",
        # other languages
        "namespace_body",
        "module",
    ]
)

# Identifier-role node types
_NAME_NODE_TYPES: frozenset[str] = frozenset(["identifier", "type_identifier"])

# Parent node types whose "name" field child is a declaration of a new name
_DECLARATION_PARENTS: frozenset[str] = frozenset(
    [
        "variable_declarator",
        "method_declaration",
        "constructor_declaration",
        "class_declaration",
        "interface_declaration",
        "enum_declaration",
        "record_declaration",
        "formal_parameter",
        "catch_formal_parameter",
        "enhanced_for_statement",  # the loop variable
        "annotation_type_element_declaration",
    ]
)

# ---------------------------------------------------------------------------
# Plugin import (one-way: plugins → emitter, not back)
# ---------------------------------------------------------------------------

from query.datalog_plugins import (  # noqa: E402
    CallExtractionStrategy,
    DatalogLanguagePlugin,
    make_default_registry,
)
from repo_surveyor.integration_patterns.types import Language  # noqa: E402

_NULL_PLUGIN: DatalogLanguagePlugin = make_default_registry().get(Language.JAVA)  # type: ignore[assignment]


# ---------------------------------------------------------------------------
# Fact dataclasses
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class NodeFact:
    node_id: int
    node_type: str

    def __str__(self) -> str:
        return f'node({self.node_id}, "{self.node_type}").'


@dataclass(frozen=True)
class TokenFact:
    node_id: int
    text: str

    def __str__(self) -> str:
        esc = self.text.replace('"', '\\"').replace("\n", "\\n").replace("\t", "\\t")
        return f'token({self.node_id}, "{esc}").'


@dataclass(frozen=True)
class PositionFact:
    node_id: int
    start_row: int
    start_col: int
    end_row: int
    end_col: int

    def __str__(self) -> str:
        return f"position({self.node_id}, {self.start_row}, {self.start_col}, {self.end_row}, {self.end_col})."


@dataclass(frozen=True)
class ParentFact:
    child_id: int
    parent_id: int

    def __str__(self) -> str:
        return f"parent({self.child_id}, {self.parent_id})."


@dataclass(frozen=True)
class ContainsFact:
    parent_id: int
    child_id: int

    def __str__(self) -> str:
        return f"contains({self.parent_id}, {self.child_id})."


@dataclass(frozen=True)
class FieldFact:
    parent_id: int
    field_name: str
    child_id: int

    def __str__(self) -> str:
        return f'field({self.parent_id}, "{self.field_name}", {self.child_id}).'


@dataclass(frozen=True)
class NameFact:
    node_id: int
    text: str

    def __str__(self) -> str:
        return f'name({self.node_id}, "{self.text}").'


@dataclass(frozen=True)
class DeclaredTypeFact:
    node_id: int
    type_text: str

    def __str__(self) -> str:
        esc = self.type_text.replace('"', '\\"')
        return f'declared_type({self.node_id}, "{esc}").'


@dataclass(frozen=True)
class ScopeFact:
    scope_id: int
    kind: str

    def __str__(self) -> str:
        return f'scope({self.scope_id}, "{self.kind}").'


@dataclass(frozen=True)
class ScopeParentFact:
    inner_id: int
    outer_id: int

    def __str__(self) -> str:
        return f"scope_parent({self.inner_id}, {self.outer_id})."


@dataclass(frozen=True)
class DeclaresFact:
    scope_id: int
    decl_id: int

    def __str__(self) -> str:
        return f"declares({self.scope_id}, {self.decl_id})."


@dataclass(frozen=True)
class DeclarationFact:
    node_id: int

    def __str__(self) -> str:
        return f"declaration({self.node_id})."


@dataclass(frozen=True)
class ReferenceFact:
    node_id: int

    def __str__(self) -> str:
        return f"reference({self.node_id})."


@dataclass(frozen=True)
class RefersToFact:
    ref_id: int
    decl_id: int

    def __str__(self) -> str:
        return f"refers_to({self.ref_id}, {self.decl_id})."


@dataclass(frozen=True)
class CallFact:
    site_id: int
    receiver_id: int | None
    method_name: str

    def __str__(self) -> str:
        recv = str(self.receiver_id) if self.receiver_id is not None else "none"
        return f'call({self.site_id}, {recv}, "{self.method_name}").'


@dataclass(frozen=True)
class InstantiationFact:
    site_id: int
    type_name: str

    def __str__(self) -> str:
        return f'instantiation({self.site_id}, "{self.type_name}").'


# ---------------------------------------------------------------------------
# Fact set container
# ---------------------------------------------------------------------------


@dataclass
class RichDatalogFactSet:
    nodes: list[NodeFact] = field(default_factory=list)
    tokens: list[TokenFact] = field(default_factory=list)
    positions: list[PositionFact] = field(default_factory=list)
    parents: list[ParentFact] = field(default_factory=list)
    contains: list[ContainsFact] = field(default_factory=list)
    fields: list[FieldFact] = field(default_factory=list)
    names: list[NameFact] = field(default_factory=list)
    declared_types: list[DeclaredTypeFact] = field(default_factory=list)
    scopes: list[ScopeFact] = field(default_factory=list)
    scope_parents: list[ScopeParentFact] = field(default_factory=list)
    declares: list[DeclaresFact] = field(default_factory=list)
    declarations: list[DeclarationFact] = field(default_factory=list)
    references: list[ReferenceFact] = field(default_factory=list)
    refers_to: list[RefersToFact] = field(default_factory=list)
    calls: list[CallFact] = field(default_factory=list)
    instantiations: list[InstantiationFact] = field(default_factory=list)

    def all_facts(self) -> Iterator:
        for bucket in [
            self.nodes,
            self.tokens,
            self.positions,
            self.parents,
            self.contains,
            self.fields,
            self.names,
            self.declared_types,
            self.scopes,
            self.scope_parents,
            self.declares,
            self.declarations,
            self.references,
            self.refers_to,
            self.calls,
            self.instantiations,
        ]:
            yield from bucket

    def to_souffle_facts(
        self,
        output_dir: Path,
        plugin: DatalogLanguagePlugin = _NULL_PLUGIN,
    ) -> None:
        """Write one TSV .facts file per relation for Soufflé consumption.

        Soufflé reads facts from tab-separated files named <relation>.facts.
        Symbol values are double-quoted to handle spaces and special characters.
        The ``call`` relation uses -1 as sentinel for a missing receiver.

        Also writes three bridge-fact files derived from *plugin*:
        ``callable_node_type.facts``, ``call_node_type.facts``,
        ``instantiation_node_type.facts``.
        """
        output_dir.mkdir(parents=True, exist_ok=True)

        def sym(s: str) -> str:
            # Soufflé facts are tab-separated; replace tabs and newlines so
            # they don't corrupt the column structure.  No outer quoting —
            # Soufflé reads unquoted strings as-is and quoted strings include
            # the quote characters in the stored symbol value.
            return s.replace("\t", " ").replace("\n", "\\n")

        def write(name: str, rows: list[str]) -> None:
            (output_dir / f"{name}.facts").write_text(
                "\n".join(rows) + ("\n" if rows else ""), encoding="utf-8"
            )

        write("node", [f"{f.node_id}\t{sym(f.node_type)}" for f in self.nodes])
        write("token", [f"{f.node_id}\t{sym(f.text)}" for f in self.tokens])
        write(
            "position",
            [
                f"{f.node_id}\t{f.start_row}\t{f.start_col}\t{f.end_row}\t{f.end_col}"
                for f in self.positions
            ],
        )
        write("parent", [f"{f.child_id}\t{f.parent_id}" for f in self.parents])
        write("contains", [f"{f.parent_id}\t{f.child_id}" for f in self.contains])
        write(
            "field",
            [f"{f.parent_id}\t{sym(f.field_name)}\t{f.child_id}" for f in self.fields],
        )
        write("name", [f"{f.node_id}\t{sym(f.text)}" for f in self.names])
        write(
            "declared_type",
            [f"{f.node_id}\t{sym(f.type_text)}" for f in self.declared_types],
        )
        write("scope", [f"{f.scope_id}\t{sym(f.kind)}" for f in self.scopes])
        write(
            "scope_parent", [f"{f.inner_id}\t{f.outer_id}" for f in self.scope_parents]
        )
        write("declares", [f"{f.scope_id}\t{f.decl_id}" for f in self.declares])
        write("declaration", [f"{f.node_id}" for f in self.declarations])
        write("reference", [f"{f.node_id}" for f in self.references])
        write("refers_to", [f"{f.ref_id}\t{f.decl_id}" for f in self.refers_to])
        write(
            "call",
            [
                f"{f.site_id}\t{-1 if f.receiver_id is None else f.receiver_id}\t{sym(f.method_name)}"
                for f in self.calls
            ],
        )
        write(
            "instantiation",
            [f"{f.site_id}\t{sym(f.type_name)}" for f in self.instantiations],
        )

        # Bridge facts — one node-type symbol per line, no extra columns.
        write(
            "callable_node_type", [sym(t) for t in sorted(plugin.callable_node_types)]
        )
        write("call_node_type", [sym(t) for t in sorted(plugin.call_node_types)])
        write(
            "instantiation_node_type",
            [sym(t) for t in sorted(plugin.instantiation_node_types)],
        )

    def summary(self) -> str:
        return "\n".join(
            [
                f"  node:          {len(self.nodes)}",
                f"  token:         {len(self.tokens)}",
                f"  position:      {len(self.positions)}",
                f"  parent:        {len(self.parents)}",
                f"  contains:      {len(self.contains)}",
                f"  field:         {len(self.fields)}",
                f"  name:          {len(self.names)}",
                f"  declared_type: {len(self.declared_types)}",
                f"  scope:         {len(self.scopes)}",
                f"  scope_parent:  {len(self.scope_parents)}",
                f"  declares:      {len(self.declares)}",
                f"  declaration:   {len(self.declarations)}",
                f"  reference:     {len(self.references)}",
                f"  refers_to:     {len(self.refers_to)}",
                f"  call:          {len(self.calls)}",
                f"  instantiation: {len(self.instantiations)}",
            ]
        )


# ---------------------------------------------------------------------------
# Emitter
# ---------------------------------------------------------------------------


class _Emitter:
    """Stateful DFS walker that builds a RichDatalogFactSet."""

    def __init__(
        self,
        source: bytes,
        scope_openers: frozenset[str],
        declaration_parents: frozenset[str],
        call_node_types: frozenset[str] = frozenset(["method_invocation"]),
        instantiation_node_types: frozenset[str] = frozenset(
            ["object_creation_expression"]
        ),
        call_field_mapping: object = None,
    ) -> None:
        from query.datalog_plugins import CallExtractionStrategy, CallFieldMapping

        self._source = source
        self._scope_openers = scope_openers
        self._declaration_parents = declaration_parents
        self._call_node_types = call_node_types
        self._instantiation_node_types = instantiation_node_types
        self._call_field_mapping: CallFieldMapping = (
            call_field_mapping
            if call_field_mapping is not None
            else CallFieldMapping(mode=CallExtractionStrategy.DIRECT_FIELDS)
        )
        self._facts = RichDatalogFactSet()
        self._id_gen = count(0)
        self._ts_to_id: dict[int, int] = {}
        self._id_to_type: dict[int, str] = {}

        # For refers_to post-pass:
        # scope_id → {name_text: decl_node_id}  (first decl wins per name per scope)
        self._scope_decls: dict[int, dict[str, int]] = defaultdict(dict)
        # scope_id → parent_scope_id
        self._scope_parent_map: dict[int, int] = {}
        # pending references: (ref_id, name_text, scope_id_at_visit)
        self._pending_refs: list[tuple[int, str, int]] = []

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _new_id(self, node: Node) -> int:
        our_id = next(self._id_gen)
        self._ts_to_id[node.id] = our_id
        self._id_to_type[our_id] = node.type
        return our_id

    def _text(self, node: Node) -> str:
        return self._source[node.start_byte : node.end_byte].decode(
            "utf-8", errors="replace"
        )

    def _child_id(self, node: Node) -> int | None:
        return self._ts_to_id.get(node.id)

    # ------------------------------------------------------------------
    # Walk
    # ------------------------------------------------------------------

    def _walk(
        self,
        node: Node,
        parent_id: int | None,
        field_name: str | None,
        scope_stack: list[int],
    ) -> None:
        our_id = self._new_id(node)
        f = self._facts

        # node / token / position
        f.nodes.append(NodeFact(our_id, node.type))
        if node.child_count == 0:
            f.tokens.append(TokenFact(our_id, self._text(node)))
        f.positions.append(
            PositionFact(
                our_id,
                node.start_point[0],
                node.start_point[1],
                node.end_point[0],
                node.end_point[1],
            )
        )

        # parent / contains / field
        if parent_id is not None:
            f.parents.append(ParentFact(our_id, parent_id))
            if node.is_named:
                f.contains.append(ContainsFact(parent_id, our_id))
            if field_name:
                f.fields.append(FieldFact(parent_id, field_name, our_id))

        # declared_type: when a node occupies the "type" field of its parent,
        # emit declared_type(parent_id, type_text).
        if field_name == "type" and parent_id is not None:
            f.declared_types.append(DeclaredTypeFact(parent_id, self._text(node)))

        # ---- scope management ----
        pre_push_scope: int | None = scope_stack[-1] if scope_stack else None
        opens_scope = node.type in self._scope_openers
        if opens_scope:
            f.scopes.append(ScopeFact(our_id, node.type))
            if pre_push_scope is not None:
                f.scope_parents.append(ScopeParentFact(our_id, pre_push_scope))
                self._scope_parent_map[our_id] = pre_push_scope
            scope_stack = scope_stack + [our_id]  # immutable push

        current_scope: int = scope_stack[-1] if scope_stack else our_id

        # ---- name / declaration / reference ----
        if node.type in _NAME_NODE_TYPES and node.child_count == 0:
            text = self._text(node)
            f.names.append(NameFact(our_id, text))

            parent_type = (
                self._id_to_type.get(parent_id, "") if parent_id is not None else ""
            )
            is_decl = field_name == "name" and parent_type in self._declaration_parents

            if is_decl:
                f.declarations.append(DeclarationFact(our_id))
                f.declares.append(DeclaresFact(current_scope, our_id))
                # Register in scope for refers_to resolution.
                # First declaration of a name in a scope wins.
                self._scope_decls[current_scope].setdefault(text, our_id)
            else:
                # type_identifier nodes are always references (to a type, not a declaration).
                # Plain identifiers not in a "name"/declaration position are references.
                f.references.append(ReferenceFact(our_id))
                self._pending_refs.append((our_id, text, current_scope))

        # ---- recurse ----
        for idx, child in enumerate(node.children):
            fname = node.field_name_for_child(idx)
            self._walk(child, our_id, fname, scope_stack)

        # ---- call / instantiation (emitted after children so child IDs exist) ----
        if node.type in self._call_node_types:
            self._emit_call(node, our_id)

        if node.type in self._instantiation_node_types:
            self._emit_instantiation(node, our_id)

    # ------------------------------------------------------------------
    # Call / instantiation emission helpers
    # ------------------------------------------------------------------

    def _emit_call(self, node: Node, our_id: int) -> None:
        from query.datalog_plugins import CallExtractionStrategy

        mapping = self._call_field_mapping
        if mapping.mode == CallExtractionStrategy.DIRECT_FIELDS:
            self._emit_call_direct_fields(node, our_id)
        else:
            self._emit_call_function_child(node, our_id)

    def _emit_call_direct_fields(self, node: Node, our_id: int) -> None:
        receiver_id: int | None = None
        method_name: str | None = None
        mapping = self._call_field_mapping
        for idx, child in enumerate(node.children):
            fname = node.field_name_for_child(idx)
            if fname == mapping.receiver:
                receiver_id = self._child_id(child)
            elif fname == mapping.method:
                method_name = self._text(child)
        if method_name is not None:
            self._facts.calls.append(CallFact(our_id, receiver_id, method_name))

    def _emit_call_function_child(self, node: Node, our_id: int) -> None:
        mapping = self._call_field_mapping
        function_child: Node | None = None
        for idx, child in enumerate(node.children):
            if node.field_name_for_child(idx) == mapping.function_field:
                function_child = child
                break
        if function_child is None:
            # Bare call with no function child — emit with no receiver.
            method_name = self._text(node)
            self._facts.calls.append(CallFact(our_id, None, method_name))
            return

        receiver_id: int | None = None
        method_name: str | None = None
        for idx, child in enumerate(function_child.children):
            fname = function_child.field_name_for_child(idx)
            if fname == mapping.receiver_subfield:
                receiver_id = self._child_id(child)
            elif fname == mapping.method_subfield:
                method_name = self._text(child)

        if method_name is None:
            # Simple call (no attribute access) — entire function child is the callee.
            method_name = self._text(function_child)

        self._facts.calls.append(CallFact(our_id, receiver_id, method_name))

    def _emit_instantiation(self, node: Node, our_id: int) -> None:
        for idx, child in enumerate(node.children):
            if node.field_name_for_child(idx) == "type":
                self._facts.instantiations.append(
                    InstantiationFact(our_id, self._text(child))
                )
                return

    # ------------------------------------------------------------------
    # Post-pass: refers_to resolution
    # ------------------------------------------------------------------

    def _resolve_refs(self) -> None:
        """Approximate name resolution: walk scope chain, first match wins."""
        for ref_id, name_text, scope_id in self._pending_refs:
            current = scope_id
            while current is not None:
                decl_id = self._scope_decls.get(current, {}).get(name_text)
                if decl_id is not None:
                    self._facts.refers_to.append(RefersToFact(ref_id, decl_id))
                    break
                current = self._scope_parent_map.get(current)

    # ------------------------------------------------------------------
    # Public entry point
    # ------------------------------------------------------------------

    def emit(self, root: Node) -> RichDatalogFactSet:
        self._walk(root, parent_id=None, field_name=None, scope_stack=[])
        self._resolve_refs()
        return self._facts


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def emit_datalog(
    root: Node,
    source: bytes,
    scope_openers: frozenset[str] = _SCOPE_OPENERS,
    declaration_parents: frozenset[str] = _DECLARATION_PARENTS,
    plugin: DatalogLanguagePlugin = _NULL_PLUGIN,
) -> RichDatalogFactSet:
    """Walk a Tree-sitter parse tree and emit the rich Datalog ontology.

    Args:
        root:               Root node of the parse tree.
        source:             Original source bytes for token text extraction.
        scope_openers:      Node types that introduce a new lexical scope.
        declaration_parents: Parent node types whose 'name' field child is a declaration.
        plugin:             Language plugin supplying call/instantiation node types and
                            field mappings.  Defaults to Java behaviour (backward compat).

    Returns:
        RichDatalogFactSet with all relations populated.
    """
    effective_scope_openers = scope_openers | plugin.extra_scope_openers
    effective_declaration_parents = (
        declaration_parents | plugin.extra_declaration_parents
    )
    return _Emitter(
        source,
        effective_scope_openers,
        effective_declaration_parents,
        call_node_types=plugin.call_node_types,
        instantiation_node_types=plugin.instantiation_node_types,
        call_field_mapping=plugin.call_field_mapping,
    ).emit(root)


def run_souffle(
    dl_path: Path,
    facts_dir: Path,
    output_dir: Path,
) -> dict[str, list[tuple[str, ...]]]:
    """Run Soufflé on *dl_path*, loading facts from *facts_dir*.

    Writes output relations to *output_dir* and returns them as a dict
    mapping relation name → list of tuples (all values are strings).

    Raises ``subprocess.CalledProcessError`` if Soufflé exits non-zero.
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        ["souffle", "-F", str(facts_dir), "-D", str(output_dir), str(dl_path)],
        check=True,
        capture_output=True,
        text=True,
    )
    return {
        csv_file.stem: [
            tuple(line.split("\t"))
            for line in csv_file.read_text(encoding="utf-8").splitlines()
            if line
        ]
        for csv_file in sorted(output_dir.glob("*.csv"))
    }


def print_facts(facts: RichDatalogFactSet, show_tokens: bool = True) -> None:
    sections = [
        ("% --- node/2 ---", facts.nodes),
        ("% --- token/2 ---", facts.tokens if show_tokens else []),
        ("% --- position/5 ---", facts.positions),
        ("% --- parent/2 ---", facts.parents),
        ("% --- contains/2 ---", facts.contains),
        ("% --- field/3 ---", facts.fields),
        ("% --- name/2 ---", facts.names),
        ("% --- declared_type/2 ---", facts.declared_types),
        ("% --- scope/2 ---", facts.scopes),
        ("% --- scope_parent/2 ---", facts.scope_parents),
        ("% --- declares/2 ---", facts.declares),
        ("% --- declaration/1 ---", facts.declarations),
        ("% --- reference/1 ---", facts.references),
        ("% --- refers_to/2 ---", facts.refers_to),
        ("% --- call/3 ---", facts.calls),
        ("% --- instantiation/2 ---", facts.instantiations),
    ]
    for header, fact_list in sections:
        if not fact_list:
            continue
        print(header)
        for f in fact_list:
            print(f"  {f}")
        print()


# ---------------------------------------------------------------------------
# Demo
# ---------------------------------------------------------------------------

_SAMPLE_JAVA = textwrap.dedent("""\
    public class OrderService {

        private final OrderRepository repo;

        public OrderService(OrderRepository repo) {
            this.repo = repo;
        }

        public Order findById(long id) {
            return repo.findById(id)
                       .orElseThrow(() -> new OrderNotFoundException(id));
        }

        public Order createOrder(String customerId, List<Item> items) {
            if (items.isEmpty()) {
                throw new IllegalArgumentException("items must not be empty");
            }
            Order order = new Order(customerId, items);
            repo.save(order);
            return order;
        }
    }
""")


def _lookup_name(facts: RichDatalogFactSet, node_id: int) -> str:
    for n in facts.names:
        if n.node_id == node_id:
            return n.text
    for t in facts.tokens:
        if t.node_id == node_id:
            return t.text
    return f"#{node_id}"


def _lookup_type(facts: RichDatalogFactSet, node_id: int) -> str:
    for n in facts.nodes:
        if n.node_id == node_id:
            return n.node_type
    return "?"


def _lookup_pos(facts: RichDatalogFactSet, node_id: int) -> str:
    for p in facts.positions:
        if p.node_id == node_id:
            return f"line {p.start_row + 1}"
    return "?"


def main() -> None:
    parser = get_parser("java")
    source = _SAMPLE_JAVA.encode("utf-8")
    tree = parser.parse(source)

    print("=== Source ===")
    print(_SAMPLE_JAVA)

    facts = emit_datalog(tree.root_node, source)

    print("=== Fact counts ===")
    print(facts.summary())

    print("\n=== Scopes and their chain ===")
    scope_ids = {f.scope_id for f in facts.scopes}
    scope_kind = {f.scope_id: f.kind for f in facts.scopes}
    scope_par = {f.inner_id: f.outer_id for f in facts.scope_parents}
    for sc in sorted(scope_ids):
        parent_str = f" → parent={scope_par[sc]}" if sc in scope_par else " (root)"
        print(
            f"  scope [{sc:3d}] {scope_kind[sc]:<30s}  {_lookup_pos(facts, sc)}{parent_str}"
        )

    print("\n=== Declarations ===")
    decl_ids = {f.node_id for f in facts.declarations}
    decl_scope = {f.decl_id: f.scope_id for f in facts.declares}
    for d in facts.declarations:
        sc = decl_scope.get(d.node_id, "?")
        sc_kind = scope_kind.get(sc, "?") if isinstance(sc, int) else "?"
        print(
            f"  [{d.node_id:3d}] name={_lookup_name(facts, d.node_id)!r:<18s}  in scope [{sc}] {sc_kind}"
        )

    print("\n=== Call graph ===")
    for c in facts.calls:
        recv = (
            _lookup_name(facts, c.receiver_id)
            if c.receiver_id is not None
            else "<none>"
        )
        recv_type = (
            _lookup_type(facts, c.receiver_id) if c.receiver_id is not None else ""
        )
        print(
            f"  site [{c.site_id:3d}]  {recv}.{c.method_name}()  (receiver node type: {recv_type})"
        )

    print("\n=== Instantiations ===")
    for i in facts.instantiations:
        print(f"  site [{i.site_id:3d}]  new {i.type_name}()")

    print("\n=== declared_type facts ===")
    for dt in facts.declared_types:
        print(
            f"  node [{dt.node_id:3d}] ({_lookup_type(facts, dt.node_id)})  : {dt.type_text}"
        )

    print("\n=== refers_to (approximate name resolution) ===")
    for r in facts.refers_to:
        ref_name = _lookup_name(facts, r.ref_id)
        decl_name = _lookup_name(facts, r.decl_id)
        ref_pos = _lookup_pos(facts, r.ref_id)
        decl_pos = _lookup_pos(facts, r.decl_id)
        print(
            f"  [{r.ref_id:3d}] {ref_name!r:<12s} ({ref_pos})  →  decl [{r.decl_id}] {decl_name!r} ({decl_pos})"
        )

    print("\n=== All Datalog facts (tokens suppressed) ===")
    print_facts(facts, show_tokens=False)


if __name__ == "__main__":
    main()
