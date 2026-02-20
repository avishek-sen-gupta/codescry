"""Load pre-classified CFG role mappings from per-language JSON files.

Zero ML/Node.js dependencies — reads ``cfg_roles.json`` files from each
language's ``integration_patterns/{lang}/`` directory and converts them into
``LanguageCFGSpec`` objects keyed by ``Language`` enum members.
"""

import json
from pathlib import Path

from repo_surveyor.cfg_constructor.types import (
    VALID_SLOTS,
    ControlFlowRole,
    FieldMapping,
    LanguageCFGSpec,
    NodeCFGSpec,
)
from repo_surveyor.integration_patterns.types import Language

_PATTERNS_DIR = Path(__file__).resolve().parents[1] / "integration_patterns"

# Mapping from Language enum members to their integration_patterns directory name.
# Only languages whose directory actually exists get an entry.
_LANG_TO_DIR: dict[Language, str] = {
    Language.JAVA: "java",
    Language.PYTHON: "python",
    Language.JAVASCRIPT: "javascript",
    Language.GO: "go",
    Language.RUBY: "ruby",
    Language.RUST: "rust",
    Language.COBOL: "cobol",
    Language.TYPESCRIPT: "typescript",
    Language.CSHARP: "csharp",
    Language.CPP: "cpp",
    Language.PLI: "pli",
    Language.C: "c",
    Language.KOTLIN: "kotlin",
    Language.SCALA: "scala",
    Language.PHP: "php",
    Language.PASCAL: "pascal",
}

_ROLE_LOOKUP: dict[str, ControlFlowRole] = {
    role.value: role for role in ControlFlowRole
}

_CFG_ROLES_FILENAME = "cfg_roles.json"

_RESERVED_KEYS = frozenset({"role"})
_META_KEY = "_meta"


def _parse_field_mapping(raw: dict, role: ControlFlowRole) -> FieldMapping:
    """Extract valid semantic slots from a raw dict, filtering against ``VALID_SLOTS``."""
    allowed = VALID_SLOTS.get(role, frozenset())
    slots = {
        key: value
        for key, value in raw.items()
        if key not in _RESERVED_KEYS and key in allowed
    }
    return FieldMapping(slots=slots)


def _parse_single_spec(raw_value: str | dict) -> NodeCFGSpec:
    """Parse a single node spec from either a simple string or extended object form."""
    if isinstance(raw_value, str):
        return NodeCFGSpec(role=_ROLE_LOOKUP.get(raw_value, ControlFlowRole.LEAF))
    role = _ROLE_LOOKUP.get(raw_value.get("role", ""), ControlFlowRole.LEAF)
    return NodeCFGSpec(role=role, field_mapping=_parse_field_mapping(raw_value, role))


def _parse_node_specs(raw_specs: dict[str, str | dict]) -> dict[str, NodeCFGSpec]:
    """Convert raw JSON entries into typed ``NodeCFGSpec`` mappings.

    Accepts both simple string values (``"branch"``) and extended object values
    (``{"role": "branch", "condition": "condition"}``).
    Unknown role values are silently mapped to ``ControlFlowRole.LEAF``.
    Filters out the ``_meta`` key before parsing node types.
    """
    return {
        node_type: _parse_single_spec(raw_value)
        for node_type, raw_value in raw_specs.items()
        if node_type != _META_KEY
    }


def _parse_meta(raw_specs: dict) -> dict:
    """Extract the ``_meta`` section from raw JSON specs.

    Returns an empty dict if ``_meta`` is absent.
    """
    meta = raw_specs.get(_META_KEY, {})
    return meta if isinstance(meta, dict) else {}


def _load_language_spec(language: Language, patterns_dir: Path) -> LanguageCFGSpec:
    """Load CFG role spec for a single language from its directory.

    Returns an empty null-object spec if the language has no directory or no
    ``cfg_roles.json`` file.
    """
    dir_name = _LANG_TO_DIR.get(language)
    if dir_name is None:
        return LanguageCFGSpec(language=language, node_specs={})

    cfg_path = patterns_dir / dir_name / _CFG_ROLES_FILENAME
    if not cfg_path.exists():
        return LanguageCFGSpec(language=language, node_specs={})

    raw_specs = json.loads(cfg_path.read_text())
    meta = _parse_meta(raw_specs)
    return LanguageCFGSpec(
        language=language,
        node_specs=_parse_node_specs(raw_specs),
        switch_fallthrough=bool(meta.get("switch_fallthrough", False)),
    )


def load_cfg_roles(
    patterns_dir: Path = _PATTERNS_DIR,
) -> dict[Language, LanguageCFGSpec]:
    """Load all CFG role specs for languages that have a ``cfg_roles.json``.

    Scans each ``Language`` enum member, checks for
    ``{patterns_dir}/{dir_name}/cfg_roles.json``, and loads if present.

    Args:
        patterns_dir: Root directory containing per-language pattern directories.

    Returns:
        Mapping from ``Language`` to its ``LanguageCFGSpec``.
    """
    return {
        spec.language: spec
        for member in Language
        if (spec := _load_language_spec(member, patterns_dir)).node_specs
    }


def get_cfg_spec(
    language: Language,
    patterns_dir: Path = _PATTERNS_DIR,
) -> LanguageCFGSpec:
    """Return the CFG spec for a single language.

    Returns an empty null-object spec if the language has no cfg_roles.json.

    Args:
        language: The language to look up.
        patterns_dir: Root directory containing per-language pattern directories.

    Returns:
        ``LanguageCFGSpec`` with node specs, or an empty spec if not found.
    """
    return _load_language_spec(language, patterns_dir)
