"""Protocols for tree-sitter types, enabling dependency injection.

These protocols decouple the CFG builder from the concrete tree-sitter
library, allowing tests to use lightweight stub implementations.
"""

from typing import Protocol, runtime_checkable


@runtime_checkable
class TSPoint(Protocol):
    """A position in source text."""

    @property
    def row(self) -> int: ...

    @property
    def column(self) -> int: ...


@runtime_checkable
class TSNode(Protocol):
    """A node in a tree-sitter parse tree."""

    @property
    def type(self) -> str: ...

    @property
    def children(self) -> list["TSNode"]: ...

    @property
    def named_children(self) -> list["TSNode"]: ...

    @property
    def start_point(self) -> TSPoint: ...

    @property
    def end_point(self) -> TSPoint: ...

    @property
    def text(self) -> bytes | None: ...

    def child_by_field_name(self, name: str) -> "TSNode | None": ...


@runtime_checkable
class TSTree(Protocol):
    """A tree-sitter parse tree."""

    @property
    def root_node(self) -> TSNode: ...


@runtime_checkable
class TSParser(Protocol):
    """A tree-sitter parser."""

    def parse(self, source: bytes) -> TSTree: ...


class ParserFactory(Protocol):
    """Factory that produces a TSParser for a given language name."""

    def __call__(self, language: str) -> TSParser: ...
