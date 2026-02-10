"""Data types for call-flow extraction results."""

from dataclasses import dataclass


@dataclass(frozen=True)
class CallTree:
    """An extracted call tree rooted at a single entry method.

    Attributes:
        entry_method: The name of the root method the tree was traced from.
        edges: Mapping of caller method name to the set of callee method names.
    """

    entry_method: str
    edges: dict[str, frozenset[str]]
