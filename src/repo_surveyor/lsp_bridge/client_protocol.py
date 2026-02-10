"""Protocol defining the LSP bridge client interface for dependency injection."""

from typing import Protocol

from .types import DocumentSymbol, Location


class LspBridgeClient(Protocol):
    """Protocol for clients that communicate with an LSP bridge server."""

    def start_server(self, language: str, root_uri: str) -> dict: ...

    def stop_server(self) -> None: ...

    def open_document(self, uri: str, language_id: str, text: str) -> None: ...

    def close_document(self, uri: str) -> None: ...

    def get_symbols(self, uri: str) -> list[DocumentSymbol]: ...

    def get_definition(
        self, uri: str, line: int, character: int
    ) -> list[Location]: ...
