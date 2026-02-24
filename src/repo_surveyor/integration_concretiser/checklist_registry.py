"""Load and index JSON evidence checklists for predicate lookup.

A checklist defines which predicates to evaluate for a given signal and
the weight each matched predicate contributes to the score adjustment.
Supports per-pattern overrides and a default fallback checklist.

An empty path disables evidence verification entirely (backward-compatible
no-op mode).
"""

import json
import logging
from pathlib import Path

from repo_surveyor.integration_concretiser.evidence_predicates import (
    ChecklistEntry,
    PredicateName,
)

logger = logging.getLogger(__name__)

_EMPTY_CHECKLIST: tuple[ChecklistEntry, ...] = ()


def _parse_entries(raw_entries: list[dict]) -> tuple[ChecklistEntry, ...]:
    """Parse a list of raw JSON entries into ChecklistEntry tuples."""
    return tuple(
        ChecklistEntry(
            predicate=PredicateName(entry["predicate"]),
            weight=float(entry["weight"]),
            pattern_arg=entry.get("pattern_arg", ""),
        )
        for entry in raw_entries
    )


class ChecklistRegistry:
    """Registry that loads a JSON checklist file and provides predicate lookup.

    Usage::

        registry = ChecklistRegistry(Path("data/evidence_checklists/default.json"))
        checklist = registry.get_checklist("common", "http_url_pattern")
    """

    def __init__(self, checklist_path: Path) -> None:
        self._default_predicates: tuple[ChecklistEntry, ...] = _EMPTY_CHECKLIST
        self._pattern_overrides: dict[str, tuple[ChecklistEntry, ...]] = {}
        self._load(checklist_path)

    def _load(self, checklist_path: Path) -> None:
        """Load and parse the JSON checklist file."""
        try:
            data = json.loads(checklist_path.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError) as exc:
            logger.error(
                "Failed to load checklist from %s: %s — using empty checklist",
                checklist_path,
                exc,
            )
            return

        version = data.get("version", "unknown")
        logger.info("Loaded evidence checklist v%s from %s", version, checklist_path)

        raw_defaults = data.get("default_predicates", [])
        self._default_predicates = _parse_entries(raw_defaults)
        logger.info("Default checklist: %d predicates", len(self._default_predicates))

        raw_overrides = data.get("pattern_overrides", {})
        self._pattern_overrides = {
            key: _parse_entries(entries) for key, entries in raw_overrides.items()
        }
        if self._pattern_overrides:
            logger.info("Pattern overrides: %d patterns", len(self._pattern_overrides))

    def get_checklist(
        self, source: str, matched_pattern: str
    ) -> tuple[ChecklistEntry, ...]:
        """Look up the checklist for a given signal source and pattern.

        Tries ``source:matched_pattern`` first, then ``matched_pattern``
        alone, then falls back to the default checklist.

        Args:
            source: The signal's source identifier (e.g. "common", "java").
            matched_pattern: The regex pattern name that matched.

        Returns:
            Tuple of ChecklistEntry objects to evaluate.
        """
        qualified_key = f"{source}:{matched_pattern}"
        override = self._pattern_overrides.get(qualified_key)
        if override is not None:
            return override

        override = self._pattern_overrides.get(matched_pattern)
        if override is not None:
            return override

        return self._default_predicates
