"""Load pre-classified CFG role mappings from per-language JSON files.

Zero ML/Node.js dependencies — reads ``cfg_roles.json`` files from each
language's ``integration_patterns/{lang}/`` directory and converts them into
``LanguageCFGSpec`` objects keyed by ``Language`` enum members.
"""

import json
from pathlib import Path

from repo_surveyor.cfg_constructor.types import ControlFlowRole, LanguageCFGSpec
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
}

_ROLE_LOOKUP: dict[str, ControlFlowRole] = {
    role.value: role for role in ControlFlowRole
}

_CFG_ROLES_FILENAME = "cfg_roles.json"


def _parse_node_specs(raw_specs: dict[str, str]) -> dict[str, ControlFlowRole]:
    """Convert ``{node_type: role_value}`` strings into typed role mappings.

    Unknown role values are silently mapped to ``ControlFlowRole.LEAF``.
    """
    return {
        node_type: _ROLE_LOOKUP.get(role_str, ControlFlowRole.LEAF)
        for node_type, role_str in raw_specs.items()
    }


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
    return LanguageCFGSpec(
        language=language,
        node_specs=_parse_node_specs(raw_specs),
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
