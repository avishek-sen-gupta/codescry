"""Join integration signals to their containing code symbols.

Pure function module — no side effects, no I/O. Resolves integration
signals to CTags symbols using line-range containment, producing
per-symbol integration profiles.
"""

import json
from dataclasses import dataclass
from itertools import groupby
from operator import attrgetter

from .ctags import CTagsEntry, CTagsResult
from .integration_detector import (
    EntityType,
    IntegrationDetectorResult,
    IntegrationSignal,
)


@dataclass(frozen=True)
class SymbolIntegration:
    """An integration signal resolved to its containing code symbol."""

    symbol_id: str
    symbol_name: str
    symbol_kind: str
    signal: IntegrationSignal


@dataclass(frozen=True)
class SymbolIntegrationProfile:
    """All integration signals for a single symbol."""

    symbol_id: str
    symbol_name: str
    symbol_kind: str
    symbol_path: str
    integrations: tuple[SymbolIntegration, ...]


@dataclass(frozen=True)
class ResolutionResult:
    """Result of resolving integration signals to symbols."""

    resolved: tuple[SymbolIntegration, ...]
    unresolved: tuple[IntegrationSignal, ...]
    profiles: tuple[SymbolIntegrationProfile, ...]

    def to_json(self, indent: int | None = 2) -> str:
        """Generate a JSON representation of the result."""
        return json.dumps(
            {
                "resolved_count": len(self.resolved),
                "unresolved_count": len(self.unresolved),
                "profiles": [
                    {
                        "symbol_id": p.symbol_id,
                        "symbol_name": p.symbol_name,
                        "symbol_kind": p.symbol_kind,
                        "symbol_path": p.symbol_path,
                        "integration_count": len(p.integrations),
                        "integration_types": sorted(
                            {si.signal.integration_type.value for si in p.integrations}
                        ),
                    }
                    for p in self.profiles
                ],
            },
            indent=indent,
        )


_EMPTY_RESULT = ResolutionResult(resolved=(), unresolved=(), profiles=())


@dataclass(frozen=True)
class _SymbolSpan:
    """A symbol's line range within a file."""

    start: int
    end: int
    symbol_id: str
    name: str
    kind: str


class _SymbolLineIndex:
    """Maps (file_path, line_number) to the most specific containing symbol.

    Construction groups CTagsEntry objects by file path, infers missing
    end lines from sibling symbols, and sorts by span width so that the
    narrowest (most specific) match is found first.
    """

    def __init__(self, entries: list[CTagsEntry]) -> None:
        self._index: dict[str, list[_SymbolSpan]] = _build_index(entries)

    def resolve(self, rel_path: str, line: int) -> _SymbolSpan | None:
        """Find the most specific (narrowest) symbol containing the given line."""
        spans = self._index.get(rel_path, [])
        containing = [s for s in spans if s.start <= line <= s.end]
        return min(containing, key=lambda s: s.end - s.start) if containing else None


def _build_index(entries: list[CTagsEntry]) -> dict[str, list[_SymbolSpan]]:
    """Build a spatial index from CTags entries grouped by file."""
    by_file: dict[str, list[CTagsEntry]] = {}
    for entry in entries:
        if entry.line is None:
            continue
        by_file.setdefault(entry.path, []).append(entry)

    return {
        path: _build_file_spans(file_entries) for path, file_entries in by_file.items()
    }


def _build_file_spans(entries: list[CTagsEntry]) -> list[_SymbolSpan]:
    """Build sorted symbol spans for a single file.

    Symbols with an explicit ``end`` use it directly. Symbols without
    ``end`` get the next symbol's start - 1 as a heuristic boundary,
    or a large sentinel if they are the last symbol in the file.
    """
    sorted_entries = sorted(entries, key=lambda e: e.line)
    spans: list[_SymbolSpan] = []

    for i, entry in enumerate(sorted_entries):
        symbol_id = f"{entry.path}:{entry.name}:{entry.kind}:{entry.line}"
        if entry.end is not None:
            end = entry.end
        elif i + 1 < len(sorted_entries):
            end = sorted_entries[i + 1].line - 1
        else:
            end = entry.line + 10_000

        spans.append(
            _SymbolSpan(
                start=entry.line,
                end=end,
                symbol_id=symbol_id,
                name=entry.name,
                kind=entry.kind,
            )
        )

    # Sort by start ascending, then by span width ascending so that the
    # narrowest (most specific) symbol is checked first.
    spans.sort(key=lambda s: (s.start, s.end - s.start))
    return spans


def _strip_repo_prefix(absolute_path: str, repo_path: str) -> str:
    """Convert an absolute file path to a path relative to the repo root."""
    normalised_repo = repo_path.rstrip("/") + "/"
    if absolute_path.startswith(normalised_repo):
        return absolute_path[len(normalised_repo) :]
    return absolute_path


def _group_into_profiles(
    resolved: list[SymbolIntegration],
) -> tuple[SymbolIntegrationProfile, ...]:
    """Group resolved integrations by symbol_id into profiles."""
    sorted_resolved = sorted(resolved, key=attrgetter("symbol_id"))
    return tuple(
        SymbolIntegrationProfile(
            symbol_id=symbol_id,
            symbol_name=group[0].symbol_name,
            symbol_kind=group[0].symbol_kind,
            symbol_path=group[0].signal.match.file_path,
            integrations=tuple(group),
        )
        for symbol_id, group_iter in groupby(
            sorted_resolved, key=attrgetter("symbol_id")
        )
        if (group := list(group_iter))
    )


def resolve_integration_signals(
    ctags_result: CTagsResult,
    integration_result: IntegrationDetectorResult,
    repo_path: str,
) -> ResolutionResult:
    """Join integration signals to their containing code symbols.

    Args:
        ctags_result: Parsed CTags output with code symbol entries.
        integration_result: Detected integration signals.
        repo_path: Absolute path to the repository root, used to
                   normalise absolute file paths to relative.

    Returns:
        ResolutionResult with resolved integrations, unresolved signals,
        and per-symbol integration profiles.
    """
    if not ctags_result.entries or not integration_result.integration_points:
        return ResolutionResult(
            resolved=(),
            unresolved=tuple(integration_result.integration_points),
            profiles=(),
        )

    index = _SymbolLineIndex(ctags_result.entries)
    resolved: list[SymbolIntegration] = []
    unresolved: list[IntegrationSignal] = []

    for signal in integration_result.integration_points:
        if signal.entity_type == EntityType.DIRECTORY:
            unresolved.append(signal)
            continue

        rel_path = _strip_repo_prefix(signal.match.file_path, repo_path)
        span = index.resolve(rel_path, signal.match.line_number)

        if span is None:
            unresolved.append(signal)
        else:
            resolved.append(
                SymbolIntegration(
                    symbol_id=span.symbol_id,
                    symbol_name=span.name,
                    symbol_kind=span.kind,
                    signal=signal,
                )
            )

    profiles = _group_into_profiles(resolved)

    return ResolutionResult(
        resolved=tuple(resolved),
        unresolved=tuple(unresolved),
        profiles=profiles,
    )
