"""Resolve semantic slots to tree-sitter child nodes.

Maps ``ChildRef`` values (field name strings or positional indices) from
``FieldMapping`` entries to actual tree-sitter child nodes.
"""

from repo_surveyor.cfg_constructor.ts_protocol import TSNode
from repo_surveyor.cfg_constructor.types import ChildRef, NodeCFGSpec, SemanticSlot


def resolve_slot(ts_node: TSNode, ref: ChildRef) -> list[TSNode]:
    """Resolve a single ``ChildRef`` to a list of zero or one tree-sitter nodes.

    - ``str`` ref: uses ``ts_node.child_by_field_name(ref)``
    - ``int`` ref: uses ``ts_node.named_children[ref]`` (bounds-checked)

    Returns a single-element list on success, or an empty list if the
    child is absent or the index is out of bounds.
    """
    if isinstance(ref, str):
        child = ts_node.child_by_field_name(ref)
        return [child] if child is not None else []
    named = ts_node.named_children
    return [named[ref]] if 0 <= ref < len(named) else []


def resolve_semantic_slot(
    ts_node: TSNode, spec: NodeCFGSpec, slot: str
) -> TSNode | None:
    """Resolve a single semantic slot to a tree-sitter child, or ``None``.

    Looks up the ``ChildRef`` for *slot* in the spec's field mapping,
    then resolves it against *ts_node*.
    """
    ref = spec.field_mapping.slots.get(slot)
    if ref is None:
        return None
    children = resolve_slot(ts_node, ref)
    return children[0] if children else None


def resolve_body_children(ts_node: TSNode, spec: NodeCFGSpec) -> list[TSNode]:
    """Resolve the children to process for SEQUENCE-like roles.

    For nodes with a ``BODY`` slot in their field mapping, resolves the body
    slot first, then returns its ``named_children``.  Otherwise returns
    ``ts_node.named_children`` directly.
    """
    body_ref = spec.field_mapping.slots.get(SemanticSlot.BODY)
    if body_ref is None:
        return list(ts_node.named_children)
    body_nodes = resolve_slot(ts_node, body_ref)
    return list(body_nodes[0].named_children) if body_nodes else []
