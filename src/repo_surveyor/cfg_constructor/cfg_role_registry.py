"""Load pre-classified CFG role mappings from a static JSON config.

Zero ML/Node.js dependencies — reads ``cfg_roles.json`` produced by the
generator script and converts it into ``LanguageCFGSpec`` objects keyed by
``Language`` enum members.
"""

import json
from pathlib import Path

from repo_surveyor.cfg_constructor.types import ControlFlowRole, LanguageCFGSpec
from repo_surveyor.integration_patterns.types import Language

_DEFAULT_CONFIG_PATH = Path(__file__).parent / "cfg_roles.json"

# Manual overrides for tree-sitter JSON keys that don't match Language enum
# member names (lowercased). Keys are JSON keys, values are Language members.
_JSON_KEY_OVERRIDES: dict[str, Language] = {
    "c_sharp": Language.CSHARP,
    "cpp": Language.CPP,
    "typescript": Language.TYPESCRIPT,
    "javascript": Language.JAVASCRIPT,
}

_ROLE_LOOKUP: dict[str, ControlFlowRole] = {
    role.value: role for role in ControlFlowRole
}


def _build_language_lookup() -> dict[str, Language]:
    """Build a mapping from JSON key strings to ``Language`` enum members.

    Starts from lowercased enum member names, then applies manual overrides.
    """
    lookup: dict[str, Language] = {member.name.lower(): member for member in Language}
    lookup.update(_JSON_KEY_OVERRIDES)
    return lookup


_LANGUAGE_LOOKUP = _build_language_lookup()


def _parse_node_specs(raw_specs: dict[str, str]) -> dict[str, ControlFlowRole]:
    """Convert ``{node_type: role_value}`` strings into typed role mappings.

    Unknown role values are silently mapped to ``ControlFlowRole.LEAF``.
    """
    return {
        node_type: _ROLE_LOOKUP.get(role_str, ControlFlowRole.LEAF)
        for node_type, role_str in raw_specs.items()
    }


def load_cfg_roles(
    config_path: Path = _DEFAULT_CONFIG_PATH,
) -> dict[Language, LanguageCFGSpec]:
    """Load all CFG role specs for languages with a matching ``Language`` enum member.

    Languages present in the JSON but without a corresponding enum member are
    silently skipped.

    Args:
        config_path: Path to the ``cfg_roles.json`` file.

    Returns:
        Mapping from ``Language`` to its ``LanguageCFGSpec``.
    """
    if not config_path.exists():
        return {}

    raw = json.loads(config_path.read_text())
    languages_section = raw.get("languages", {})

    return {
        lang: LanguageCFGSpec(
            language=lang,
            node_specs=_parse_node_specs(entry.get("node_specs", {})),
        )
        for key, entry in languages_section.items()
        if (lang := _LANGUAGE_LOOKUP.get(key)) is not None
    }


def get_cfg_spec(
    language: Language,
    config_path: Path = _DEFAULT_CONFIG_PATH,
) -> LanguageCFGSpec:
    """Return the CFG spec for a single language.

    Returns an empty null-object spec if the language is not found in the config.

    Args:
        language: The language to look up.
        config_path: Path to the ``cfg_roles.json`` file.

    Returns:
        ``LanguageCFGSpec`` with node specs, or an empty spec if not found.
    """
    specs = load_cfg_roles(config_path)
    return specs.get(
        language,
        LanguageCFGSpec(language=language, node_specs={}),
    )
