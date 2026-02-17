# CFG Constructor

The `cfg_constructor` module provides a language-independent foundation for building control flow graphs (CFGs) from tree-sitter parse trees. Rather than writing a CFG builder per language, the approach uses a **control flow role schema** — a small enum of structural roles that map to CFG construction rules.

For each language, tree-sitter node types are classified into these roles, producing a static mapping stored in per-language `cfg_roles.json` files. The CFG builder dispatches on roles, not node type names, to deterministically construct a `CFGraph` from any supported language's parse tree.

## Role Schema

| Role | CFG Semantics | Examples |
|------|--------------|----------|
| `SEQUENCE` | Ordered children execute one after another | `block`, `program`, `statement_list` |
| `BRANCH` | Condition selects one of N bodies | `if_statement`, `ternary_expression` |
| `SWITCH` | Multi-arm dispatch, possible fallthrough | `switch_statement`, `match_expression` |
| `LOOP` | Body may re-execute based on condition | `while_statement`, `for_statement` |
| `LOOP_POST_CONDITION` | Body executes at least once, then condition checked | `do_statement` |
| `RETURN` | Terminates function, transfers to caller | `return_statement` |
| `BREAK` | Exits enclosing loop/switch | `break_statement` |
| `CONTINUE` | Skips to next loop iteration | `continue_statement` |
| `THROW` | Transfers to nearest exception handler | `throw_statement`, `raise` |
| `TRY` | Introduces exception-handling boundary with body + handlers + optional finally | `try_statement` |
| `CALL` | Function/method invocation (sequential for now; hook point for inter-procedural edges) | `method_invocation`, `call_expression` |
| `LEAF` | No control flow effect (default for unmapped nodes) | expressions, assignments, declarations |

## Types

- **`ControlFlowRole`** — enum with 12 members as listed above
- **`SemanticSlot`** — constants class with 9 named slot strings: `CONDITION`, `CONSEQUENCE`, `ALTERNATIVE`, `VALUE`, `BODY`, `INITIALIZER`, `UPDATE`, `HANDLER`, `FINALIZER`
- **`FieldMapping`** — frozen dataclass with `slots: dict[str, ChildRef]` mapping semantic slot names to tree-sitter child field names (strings) or positional indices (integers). Default is empty.
- **`NodeCFGSpec`** — frozen dataclass pairing a `ControlFlowRole` with an optional `FieldMapping`. Default role is `LEAF` with no field mapping.
- **`LanguageCFGSpec`** — frozen dataclass pairing a `Language` with a `dict[str, NodeCFGSpec]` mapping tree-sitter node type strings to node specs. Includes `role_for(node_type)` (returns `LEAF` for unmapped types), `spec_for(node_type)` (returns full `NodeCFGSpec`, null-object for unmapped types), `is_mapped(node_type)` (checks if a node type has an explicit entry), and a `switch_fallthrough: bool` flag indicating whether switch cases fall through by default (True for C, C++, Java, JavaScript, TypeScript, PHP; False for C#, Python, etc.)
- **`VALID_SLOTS`** — dict mapping `ControlFlowRole` to `frozenset[str]` of valid semantic slot names for that role. Only `BRANCH`, `SWITCH`, `LOOP`, `LOOP_POST_CONDITION`, and `TRY` have slots; other roles have none.

## CFG Graph Types (`cfg_types.py`)

- **`EdgeKind`** — enum: `NORMAL`, `TRUE`, `FALSE`, `EXCEPTION`, `BACK`, `FALLTHROUGH`
- **`CFGNode`** — frozen dataclass representing a graph node with `id`, `node_type`, `role`, `start_point`, `end_point`, `text_snippet`
- **`CFGEdge`** — frozen dataclass with `source`, `target`, `kind`
- **`CFGraph`** — frozen dataclass holding `nodes`, `edges`, `entry` (ENTRY sentinel ID), `exit` (EXIT sentinel ID)
- **`PendingEdge`** — frozen dataclass for unresolved edges that bubble up through the recursive construction, with `source_id` and optional `label` (for labeled break/continue)
- **`Fragment`** — frozen dataclass representing a mini-CFG from a subtree, with `entry`, `exits`, and pending collections (`pending_breaks`, `pending_continues`, `pending_throws`, `pending_returns`)
- **`EMPTY_FRAGMENT`** — sentinel for empty subtrees (`entry=-1`, `exits=frozenset()`)

## Tree-Sitter Protocols (`ts_protocol.py`)

Protocols for dependency injection of tree-sitter types:
- **`TSNode`** — protocol matching `tree_sitter.Node` (`.type`, `.children`, `.named_children`, `.child_by_field_name()`, `.start_point`, `.end_point`, `.text`)
- **`TSTree`** — protocol with `.root_node`
- **`TSParser`** — protocol with `.parse(bytes)`
- **`ParserFactory`** — callable protocol producing a `TSParser` for a language name

## Static Config: Per-Language `cfg_roles.json`

The per-language role mappings are stored as individual `cfg_roles.json` files inside each language's `integration_patterns/{lang}/` directory. The format supports two forms:

**Simple string form** — maps a node type directly to a role (for nodes that don't need child field disambiguation):

```json
{"break_statement": "break", "method_invocation": "call", "identifier": "leaf"}
```

**Extended object form** — maps a node type to a role plus semantic field mappings that identify which tree-sitter child fields serve which structural purpose:

```json
{
  "if_statement": {
    "role": "branch",
    "condition": "condition",
    "consequence": "consequence",
    "alternative": "alternative"
  }
}
```

Slot values are tree-sitter field name strings, or integers for positional (Nth named child) fallback. Both forms can be mixed freely in a single file. Invalid slot names for a given role are silently dropped during loading.

**`_meta` key** — an optional top-level key for language-level configuration:

```json
{
  "_meta": {"switch_fallthrough": true},
  "program": "sequence",
  ...
}
```

The `switch_fallthrough` flag (default `false`) controls whether the CFG builder wires switch case arms sequentially with `FALLTHROUGH` edges. Languages where `switch` cases fall through by default (C, C++, Java, JavaScript, TypeScript, PHP) set this to `true`. Languages with mandatory `break` per arm (C#) or pattern-matching semantics (Python, Rust, Go) leave it `false`.

**Semantic slots per role:**

| Role | Required | Optional |
|------|----------|----------|
| `BRANCH` | `condition`, `consequence` | `alternative` |
| `SWITCH` | — | `value`, `body` |
| `LOOP` | `body` | `condition`, `initializer`, `update` |
| `LOOP_POST_CONDITION` | `body`, `condition` | — |
| `TRY` | `body` | `handler`, `finalizer` |
| `SEQUENCE`, `LEAF`, `RETURN`, `BREAK`, `CONTINUE`, `THROW`, `CALL` | — | — |

Languages with per-language cfg_roles.json: Java, Python, JavaScript, TypeScript, Go, Ruby, Rust, C, C++, C#, Kotlin, Scala, PHP, COBOL.

These files were hand-authored by consulting each language's tree-sitter grammar (`src/node-types.json`) from the [tree-sitter-language-pack](https://github.com/Goldziher/tree-sitter-language-pack). Statement-level node types are explicitly classified; structural container nodes (class/function/method declarations) are intentionally left unmapped so the builder treats them as transparent containers and descends into their children. Unmapped types default to `leaf` at runtime via `LanguageCFGSpec.role_for()`.

Semantic field mappings (extended object form) are populated for node types with roles `BRANCH`, `SWITCH`, `LOOP`, `LOOP_POST_CONDITION`, and `TRY` across all 13 languages except COBOL (whose tree-sitter grammar uses header nodes without standard named fields). Each mapping was sourced from the respective tree-sitter grammar's `grammar.js` / `node-types.json`. Node types that lack named fields (e.g. sub-clauses like `catch_clause`) or use only positional children remain in simple string form.

## Loader API: `cfg_role_registry`

The loader module reads per-language `cfg_roles.json` files at runtime with zero ML or Node.js dependencies:

- **`load_cfg_roles(patterns_dir)`** — scans each `Language` enum member's directory for a `cfg_roles.json`, returns `dict[Language, LanguageCFGSpec]`
- **`get_cfg_spec(language, patterns_dir)`** — returns the spec for a single language; returns an empty null-object spec (all nodes map to `LEAF`) if not found

```python
from repo_surveyor.cfg_constructor import load_cfg_roles, get_cfg_spec
from repo_surveyor.integration_patterns.types import Language

# Load all available specs
specs = load_cfg_roles()
java_spec = specs[Language.JAVA]
print(java_spec.role_for("if_statement"))  # ControlFlowRole.BRANCH

# Or load a single language
spec = get_cfg_spec(Language.PYTHON)
print(spec.role_for("for_statement"))  # ControlFlowRole.LOOP
print(spec.role_for("unknown_node"))   # ControlFlowRole.LEAF
```

`Language` enum members are mapped to their `integration_patterns/` directory names via an internal `_LANG_TO_DIR` dict (e.g. `Language.CSHARP` -> `"csharp"`, `Language.CPP` -> `"cpp"`).

## Builder API: `build_cfg()`

The `build_cfg()` function in `builder.py` is the main entry point for constructing a control flow graph from source code:

```python
from tree_sitter_language_pack import get_parser
from repo_surveyor.cfg_constructor.builder import build_cfg
from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec
from repo_surveyor.integration_patterns.types import Language

spec = get_cfg_spec(Language.JAVA)
parser = get_parser("java")
source = b"""
class A {
    void m(int x) {
        if (x > 0) {
            return x;
        }
        return -x;
    }
}
"""
graph = build_cfg(source, spec, parser)

print(f"Nodes: {len(graph.nodes)}")
print(f"Edges: {len(graph.edges)}")
for edge in graph.edges:
    print(f"  {edge.source} -> {edge.target} ({edge.kind.value})")
```

The builder works in three phases:

1. **Parse**: The tree-sitter parser produces a parse tree from the source bytes.
2. **Recursive fragment construction**: Each tree-sitter node is dispatched to a role-specific handler based on its `cfg_roles.json` mapping. Each handler produces a `Fragment` — a mini-CFG with entry, exits, and pending edges (break, continue, throw, return) that bubble up to be resolved by enclosing scope handlers.
3. **Top-level wiring**: An ENTRY sentinel is wired to the root fragment's entry, normal exits and pending returns are wired to an EXIT sentinel, and unhandled throws are wired to EXIT with EXCEPTION edges.

**Unmapped node types** (not in `cfg_roles.json`) are treated as transparent containers — the builder descends through their named children without creating a CFG node. This allows structural wrapper nodes like `class_declaration` and `method_declaration` to be traversed automatically. Explicitly mapped LEAF nodes are atomic and not descended into.

**Per-role construction rules:**

| Role | Construction | Edge kinds produced |
|------|-------------|-------------------|
| `LEAF` / `CALL` | Atomic: entry = exit = self | — |
| `RETURN` / `THROW` | Terminal: no exits, pending return/throw | — |
| `BREAK` / `CONTINUE` | Terminal: no exits, pending break/continue (with optional label) | — |
| `SEQUENCE` | Chain children left-to-right; stop after dead code | `NORMAL` |
| `BRANCH` | Condition -> consequence (`TRUE`), alternative (`FALSE`) | `TRUE`, `FALSE` |
| `LOOP` | [init ->] condition ->(`TRUE`) body [-> update -> condition(`BACK`)] | `TRUE`, `BACK`, `NORMAL` |
| `LOOP_POST_CONDITION` | body -> condition ->(`BACK`) body | `NORMAL`, `BACK` |
| `SWITCH` | value -> each arm; with/without fallthrough wiring | `NORMAL`, `FALLTHROUGH` |
| `TRY` | body -> handler (via throws, `EXCEPTION`); body+handler -> finally (funnel model) | `NORMAL`, `EXCEPTION` |

## Internal Modules

- **`child_resolver.py`** — resolves semantic slots (e.g. `condition`, `body`) to tree-sitter child nodes using `FieldMapping` entries. Supports both field-name and positional-index `ChildRef` values.
- **`fragment_ops.py`** — pure functions for wiring fragments (`wire`, `chain`, `merge_exits`), creating atomic/terminal fragments (`make_atomic`, `make_terminal_return`, etc.), and resolving pending edges (`resolve_breaks`, `resolve_continues`).
- **`role_handlers.py`** — one handler function per `ControlFlowRole`, plus a `NodeFactory` that allocates monotonically increasing node IDs. The `ROLE_HANDLERS` dispatch table maps roles to handlers. Each handler takes a tree-sitter node, its spec, a factory, a recursive build callback, the language spec, and a label scope dict.

## Design Decisions

- `BREAK`/`CONTINUE`/`RETURN`/`THROW` are separate roles (not a single `EXIT`) because they target different scopes and produce different CFG edges
- `SWITCH` is separate from `BRANCH` — fallthrough semantics (C/Java `switch` vs Rust `match`) change edge structure
- `LOOP_POST_CONDITION` is separate from `LOOP` — `do...while` guarantees at least one body execution, changing the entry edge
- `CALL` is treated as sequential for now, marking call sites for future inter-procedural expansion
- `LEAF` is the safe default — unclassified nodes produce sequential edges
- Field mappings are optional per node type — the extended object form in `cfg_roles.json` allows specifying which tree-sitter child fields correspond to semantic slots (condition, body, etc.), while the simple string form remains for nodes that don't need child disambiguation
- **Unmapped = transparent**: Node types absent from `cfg_roles.json` are treated as transparent containers — the builder descends through their children without creating a CFG node. Only explicitly-mapped LEAF nodes are atomic. This avoids having to classify every possible tree-sitter node type, and lets structural wrappers like `class_declaration` and `method_declaration` be traversed naturally.
- **Statement-level granularity**: The CFG operates at statement level. Expression-level sub-trees within a statement are not decomposed into separate CFG nodes.
- **Fragment abstraction**: The recursive builder uses a `Fragment` type as its core abstraction — each subtree produces a mini-CFG with entry, exits, and pending edges. This cleanly separates local construction from scope resolution (e.g. break targets are resolved by the enclosing loop, not by the break node itself).
- **Funnel model for finally**: In TRY blocks, the finally clause intercepts ALL outgoing paths (normal exits, returns, breaks, continues, throws). Pending edges are wired through the finally entry and re-pended from its exits, ensuring finally always executes.
- **Language-level fallthrough**: Switch fallthrough is a language-level flag (`switch_fallthrough` in `_meta`), not per-node. Languages either have fallthrough semantics (C, Java) or don't (C#, Python).
