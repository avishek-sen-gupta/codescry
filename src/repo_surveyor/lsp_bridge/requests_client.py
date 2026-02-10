"""Concrete LSP bridge client using the requests library."""

from __future__ import annotations

import requests

from .types import DocumentSymbol, Location


def _parse_symbol(raw: dict) -> DocumentSymbol:
    """Recursively parse a raw DocumentSymbol dict into a typed dataclass."""
    rng = raw["range"]
    children = tuple(_parse_symbol(c) for c in raw.get("children", []))
    return DocumentSymbol(
        name=raw["name"],
        kind=raw["kind"],
        range_start_line=rng["start"]["line"],
        range_start_char=rng["start"]["character"],
        range_end_line=rng["end"]["line"],
        range_end_char=rng["end"]["character"],
        children=children,
    )


class RequestsLspBridgeClient:
    """LSP bridge client backed by HTTP requests."""

    def __init__(self, base_url: str) -> None:
        self._base_url = base_url

    def start_server(self, language: str, root_uri: str) -> dict:
        resp = requests.post(
            f"{self._base_url}/start",
            json={"language": language, "rootUri": root_uri},
        )
        resp.raise_for_status()
        return resp.json()

    def stop_server(self) -> None:
        requests.post(f"{self._base_url}/stop", json={})

    def open_document(self, uri: str, language_id: str, text: str) -> None:
        resp = requests.post(
            f"{self._base_url}/document/open",
            json={"uri": uri, "languageId": language_id, "text": text},
        )
        resp.raise_for_status()

    def close_document(self, uri: str) -> None:
        requests.post(f"{self._base_url}/document/close", json={"uri": uri})

    def get_symbols(self, uri: str) -> list[DocumentSymbol]:
        resp = requests.post(
            f"{self._base_url}/symbols", json={"uri": uri}
        )
        resp.raise_for_status()
        raw_symbols = resp.json().get("symbols") or []
        return [_parse_symbol(s) for s in raw_symbols]

    def get_definition(
        self, uri: str, line: int, character: int
    ) -> list[Location]:
        resp = requests.post(
            f"{self._base_url}/definition",
            json={"uri": uri, "line": line, "character": character},
        )
        resp.raise_for_status()
        raw_locations = resp.json().get("locations") or []
        return [
            Location(
                uri=loc["uri"],
                range_start_line=loc["range"]["start"]["line"],
                range_start_char=loc["range"]["start"]["character"],
                range_end_line=loc["range"]["end"]["line"],
                range_end_char=loc["range"]["end"]["character"],
            )
            for loc in raw_locations
        ]
