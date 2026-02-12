"""Integration patterns for detecting system integration points.

Patterns are organized by language with base patterns that always apply
and framework-specific patterns that apply when a framework is active.
"""

from .types import Confidence, IntegrationType, Language
from . import common

from ..language_plugin import PluginRegistry

_registry = PluginRegistry()

# Backward-compatible module-level exports (same shapes as old hardcoded dicts)
EXTENSION_TO_LANGUAGE = _registry.extension_to_language_enum()
LANGUAGE_MODULES = _registry.language_to_integration_module()


def get_patterns_for_language(
    language: Language | None,
    frameworks: list[str] = [],
) -> dict[IntegrationType, list[tuple[str, Confidence]]]:
    """Get integration patterns for a specific language and active frameworks.

    Combines common patterns with language-specific base patterns and
    framework-specific patterns for any active frameworks.

    Args:
        language: The programming language, or None for common patterns only.
        frameworks: List of active framework names (e.g., ["FastAPI", "Django"]).
                    Framework-specific patterns for these frameworks
                    are included alongside base patterns.

    Returns:
        Dict mapping IntegrationType to list of (pattern, confidence) tuples.
    """
    result: dict[IntegrationType, list[tuple[str, Confidence]]] = {}

    for integration_type in IntegrationType:
        patterns: list[tuple[str, Confidence]] = []

        # Add common patterns
        common_type_patterns = common.COMMON.patterns.get(integration_type, {})
        patterns.extend(common_type_patterns.get("patterns", []))

        # Add language-specific base patterns
        if language is not None:
            lang_module = LANGUAGE_MODULES.get(language)
            if lang_module is not None:
                lang_type_patterns = lang_module.BASE_PATTERNS.get(integration_type, {})
                patterns.extend(lang_type_patterns.get("patterns", []))

                # Add framework-specific patterns for active frameworks
                for framework in frameworks:
                    fw_patterns = lang_module.FRAMEWORK_PATTERNS.get(framework, {})
                    fw_type_patterns = fw_patterns.get(integration_type, {})
                    patterns.extend(fw_type_patterns.get("patterns", []))

        result[integration_type] = patterns

    return result


def get_directory_patterns() -> dict[IntegrationType, list[str]]:
    """Get directory patterns for all integration types.

    Returns:
        Dict mapping IntegrationType to list of directory patterns.
    """
    return {
        integration_type: patterns.get("directory_patterns", [])
        for integration_type, patterns in common.COMMON.patterns.items()
    }


__all__ = [
    "Confidence",
    "IntegrationType",
    "Language",
    "EXTENSION_TO_LANGUAGE",
    "LANGUAGE_MODULES",
    "get_patterns_for_language",
    "get_directory_patterns",
    "common",
]
