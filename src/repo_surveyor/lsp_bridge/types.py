"""Data types for LSP bridge responses."""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(frozen=True)
class DocumentSymbol:
    """An LSP DocumentSymbol with recursive children."""

    name: str
    kind: int
    range_start_line: int
    range_start_char: int
    range_end_line: int
    range_end_char: int
    children: tuple[DocumentSymbol, ...] = field(default_factory=tuple)


@dataclass(frozen=True)
class Location:
    """An LSP Location pointing to a position in a document."""

    uri: str
    range_start_line: int
    range_start_char: int
    range_end_line: int
    range_end_char: int
