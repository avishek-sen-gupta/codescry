"""Language plugin system for Datalog emission.

Each language provides a frozen-dataclass config; a registry looks them up by
the existing ``Language`` enum.  Language-specific node type strings are
injected as Soufflé bridge facts so ``analysis.dl`` never embeds literal
node-type strings.

Scope/declaration config is additive — plugins contribute deltas on top of the
shared defaults ``_SCOPE_OPENERS`` / ``_DECLARATION_PARENTS`` defined in
``treesitter_to_datalog``.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum

from repo_surveyor.integration_patterns.types import Language


class CallExtractionStrategy(str, Enum):
    """How to extract receiver / method-name from a call node."""

    DIRECT_FIELDS = "direct_fields"
    """Java: fields named ``object`` / ``name`` directly on the call node."""

    FUNCTION_CHILD = "function_child"
    """Python/JS/Go/Rust: descend into the ``function`` child, then read
    ``receiver_subfield`` / ``method_subfield`` from that child."""


@dataclass(frozen=True)
class CallFieldMapping:
    """Per-language field-name conventions for call nodes."""

    mode: CallExtractionStrategy
    receiver: str = "object"
    """DIRECT_FIELDS: field name for the receiver object."""
    method: str = "name"
    """DIRECT_FIELDS: field name for the method name."""
    function_field: str = "function"
    """FUNCTION_CHILD: the field that holds the function/attribute node."""
    receiver_subfield: str = "object"
    """FUNCTION_CHILD: sub-field of the function child that holds the receiver."""
    method_subfield: str = "attribute"
    """FUNCTION_CHILD: sub-field of the function child that holds the method name."""


@dataclass(frozen=True)
class DatalogLanguagePlugin:
    """All language-specific configuration needed by the Datalog emitter."""

    language: Language
    tree_sitter_language: str
    """Language name passed to ``get_parser()``."""
    extra_scope_openers: frozenset[str]
    """Unioned with ``_SCOPE_OPENERS`` defaults."""
    extra_declaration_parents: frozenset[str]
    """Unioned with ``_DECLARATION_PARENTS`` defaults."""
    callable_node_types: frozenset[str]
    """Written to ``callable_node_type.facts``."""
    call_node_types: frozenset[str]
    """Written to ``call_node_type.facts`` and used in ``_walk()`` dispatch."""
    instantiation_node_types: frozenset[str]
    """Written to ``instantiation_node_type.facts``."""
    call_field_mapping: CallFieldMapping


class DatalogPluginRegistry:
    """Registry mapping ``Language`` values to ``DatalogLanguagePlugin``s."""

    def __init__(self, plugins: list[DatalogLanguagePlugin]) -> None:
        self._registry: dict[Language, DatalogLanguagePlugin] = {
            p.language: p for p in plugins
        }

    def get(self, language: Language) -> DatalogLanguagePlugin | None:
        return self._registry.get(language)

    def all_plugins(self) -> list[DatalogLanguagePlugin]:
        return list(self._registry.values())


_DIRECT = CallFieldMapping(mode=CallExtractionStrategy.DIRECT_FIELDS)

_JAVA_PLUGIN = DatalogLanguagePlugin(
    language=Language.JAVA,
    tree_sitter_language="java",
    extra_scope_openers=frozenset(),
    extra_declaration_parents=frozenset(),
    callable_node_types=frozenset(["method_declaration", "constructor_declaration"]),
    call_node_types=frozenset(["method_invocation"]),
    instantiation_node_types=frozenset(["object_creation_expression"]),
    call_field_mapping=_DIRECT,
)

_PYTHON_PLUGIN = DatalogLanguagePlugin(
    language=Language.PYTHON,
    tree_sitter_language="python",
    extra_scope_openers=frozenset(),
    extra_declaration_parents=frozenset(),
    callable_node_types=frozenset(["function_definition"]),
    call_node_types=frozenset(["call"]),
    instantiation_node_types=frozenset(),
    call_field_mapping=CallFieldMapping(
        mode=CallExtractionStrategy.FUNCTION_CHILD,
        function_field="function",
        receiver_subfield="object",
        method_subfield="attribute",
    ),
)

_JAVASCRIPT_PLUGIN = DatalogLanguagePlugin(
    language=Language.JAVASCRIPT,
    tree_sitter_language="javascript",
    extra_scope_openers=frozenset(),
    extra_declaration_parents=frozenset(),
    callable_node_types=frozenset(
        [
            "function_declaration",
            "function_expression",
            "method_definition",
            "arrow_function",
        ]
    ),
    call_node_types=frozenset(["call_expression"]),
    instantiation_node_types=frozenset(["new_expression"]),
    call_field_mapping=CallFieldMapping(
        mode=CallExtractionStrategy.FUNCTION_CHILD,
        function_field="function",
        receiver_subfield="object",
        method_subfield="property",
    ),
)

_TYPESCRIPT_PLUGIN = DatalogLanguagePlugin(
    language=Language.TYPESCRIPT,
    tree_sitter_language="typescript",
    extra_scope_openers=frozenset(),
    extra_declaration_parents=frozenset(),
    callable_node_types=frozenset(
        [
            "function_declaration",
            "function_expression",
            "method_definition",
            "arrow_function",
        ]
    ),
    call_node_types=frozenset(["call_expression"]),
    instantiation_node_types=frozenset(["new_expression"]),
    call_field_mapping=CallFieldMapping(
        mode=CallExtractionStrategy.FUNCTION_CHILD,
        function_field="function",
        receiver_subfield="object",
        method_subfield="property",
    ),
)

_GO_PLUGIN = DatalogLanguagePlugin(
    language=Language.GO,
    tree_sitter_language="go",
    extra_scope_openers=frozenset(),
    extra_declaration_parents=frozenset(),
    callable_node_types=frozenset(["function_declaration", "method_declaration"]),
    call_node_types=frozenset(["call_expression"]),
    instantiation_node_types=frozenset(),
    call_field_mapping=CallFieldMapping(
        mode=CallExtractionStrategy.FUNCTION_CHILD,
        function_field="function",
        receiver_subfield="object",
        method_subfield="field",
    ),
)

_RUST_PLUGIN = DatalogLanguagePlugin(
    language=Language.RUST,
    tree_sitter_language="rust",
    extra_scope_openers=frozenset(),
    extra_declaration_parents=frozenset(),
    callable_node_types=frozenset(["function_item"]),
    call_node_types=frozenset(["call_expression"]),
    instantiation_node_types=frozenset(["struct_expression"]),
    call_field_mapping=CallFieldMapping(
        mode=CallExtractionStrategy.FUNCTION_CHILD,
        function_field="function",
        receiver_subfield="object",
        method_subfield="name",
    ),
)

_ALL_PLUGINS: list[DatalogLanguagePlugin] = [
    _JAVA_PLUGIN,
    _PYTHON_PLUGIN,
    _JAVASCRIPT_PLUGIN,
    _TYPESCRIPT_PLUGIN,
    _GO_PLUGIN,
    _RUST_PLUGIN,
]


def make_default_registry() -> DatalogPluginRegistry:
    """Build a registry with plugins for Java, Python, JavaScript, TypeScript, Go, Rust."""
    return DatalogPluginRegistry(_ALL_PLUGINS)
