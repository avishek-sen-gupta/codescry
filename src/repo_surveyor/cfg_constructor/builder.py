"""Public API for deterministic CFG construction from tree-sitter parse trees.

``build_cfg()`` is the main entry point: given source bytes, a language,
and a parser factory, it produces a complete ``CFGraph``.
"""

from repo_surveyor.cfg_constructor.cfg_types import (
    CFGEdge,
    CFGraph,
    EMPTY_FRAGMENT,
    EdgeKind,
    Fragment,
)
from repo_surveyor.cfg_constructor.fragment_ops import chain, wire, wire_ids
from repo_surveyor.cfg_constructor.role_handlers import (
    LabelTarget,
    NodeFactory,
    ROLE_HANDLERS,
    handle_sequence,
)
from repo_surveyor.cfg_constructor.ts_protocol import TSNode, TSParser
from repo_surveyor.cfg_constructor.types import (
    ControlFlowRole,
    LanguageCFGSpec,
    NodeCFGSpec,
)

# Spec used for transparent container nodes (unmapped) processed as sequences.
_TRANSPARENT_SPEC = NodeCFGSpec(role=ControlFlowRole.SEQUENCE)


def _build_fragment(
    ts_node: TSNode,
    lang_spec: LanguageCFGSpec,
    factory: NodeFactory,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """Recursively build a Fragment for a single tree-sitter node.

    Unmapped node types (not in ``lang_spec.node_specs``) are treated as
    transparent containers: their ``named_children`` are processed as a
    sequence without creating a CFG node for the container itself.  This
    allows the builder to descend through wrapper nodes like
    ``class_declaration`` and ``method_declaration`` to reach the actual
    control flow statements.
    """
    if not lang_spec.is_mapped(ts_node.type):
        # Transparent container: process children as sequence
        return _build_transparent(ts_node, lang_spec, factory, label_scope)

    spec = lang_spec.spec_for(ts_node.type)
    role = spec.role
    handler = ROLE_HANDLERS.get(role, ROLE_HANDLERS[ControlFlowRole.LEAF])

    def build(child: TSNode) -> tuple[Fragment, tuple[CFGEdge, ...]]:
        return _build_fragment(child, lang_spec, factory, label_scope)

    return handler(ts_node, spec, factory, build, lang_spec, label_scope)


def _build_transparent(
    ts_node: TSNode,
    lang_spec: LanguageCFGSpec,
    factory: NodeFactory,
    label_scope: dict[str, LabelTarget],
) -> tuple[Fragment, tuple[CFGEdge, ...]]:
    """Process an unmapped node's children as a sequence without creating a node."""
    children = ts_node.named_children
    if not children:
        return EMPTY_FRAGMENT, ()

    all_edges: list[CFGEdge] = []
    current = EMPTY_FRAGMENT

    for child in children:
        child_frag, child_edges = _build_fragment(
            child, lang_spec, factory, label_scope
        )
        all_edges.extend(child_edges)

        combined, chain_edges = chain(current, child_frag)
        all_edges.extend(chain_edges)
        current = combined

        if child_frag.entry != -1 and not child_frag.exits:
            break

    return current, tuple(all_edges)


def build_cfg(
    source: bytes,
    lang_spec: LanguageCFGSpec,
    parser: TSParser,
) -> CFGraph:
    """Build a complete control flow graph from source code.

    Args:
        source: Source code as bytes.
        lang_spec: Language-specific CFG spec with role mappings.
        parser: A tree-sitter parser for the target language.

    Returns:
        A ``CFGraph`` with ENTRY/EXIT sentinels and all edges resolved.

    Raises:
        ValueError: If orphan pending breaks or continues remain after
            top-level wiring (indicates a break/continue outside a loop/switch).
    """
    factory = NodeFactory()
    entry_sentinel = factory.create_sentinel("ENTRY")
    exit_sentinel = factory.create_sentinel("EXIT")

    tree = parser.parse(source)
    root = tree.root_node

    label_scope: dict[str, LabelTarget] = {}
    fragment, frag_edges = _build_fragment(root, lang_spec, factory, label_scope)

    all_edges: list[CFGEdge] = list(frag_edges)

    # Wire ENTRY -> fragment.entry
    if fragment.entry != -1:
        all_edges.extend(
            wire(frozenset({entry_sentinel.id}), fragment.entry, EdgeKind.NORMAL)
        )

    # Wire fragment.exits -> EXIT
    all_edges.extend(wire(fragment.exits, exit_sentinel.id, EdgeKind.NORMAL))

    # Wire pending returns -> EXIT
    if fragment.pending_returns:
        all_edges.extend(
            wire_ids(fragment.pending_returns, exit_sentinel.id, EdgeKind.NORMAL)
        )

    # Wire unhandled throws -> EXIT
    if fragment.pending_throws:
        all_edges.extend(
            wire_ids(fragment.pending_throws, exit_sentinel.id, EdgeKind.EXCEPTION)
        )

    # Orphan breaks/continues are an error
    if fragment.pending_breaks:
        source_ids = [pe.source_id for pe in fragment.pending_breaks]
        raise ValueError(f"Orphan pending breaks at top level: node IDs {source_ids}")
    if fragment.pending_continues:
        source_ids = [pe.source_id for pe in fragment.pending_continues]
        raise ValueError(
            f"Orphan pending continues at top level: node IDs {source_ids}"
        )

    return CFGraph(
        nodes=factory.all_nodes,
        edges=tuple(all_edges),
        entry=entry_sentinel.id,
        exit=exit_sentinel.id,
    )
