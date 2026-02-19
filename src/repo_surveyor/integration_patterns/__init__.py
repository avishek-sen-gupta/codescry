"""Integration patterns for detecting system integration points.

Patterns are organized by language with base patterns that always apply
and framework-specific patterns that apply when a framework is active.
"""

from .types import Confidence, IntegrationType, Language, PatternKey, SignalDirection
from . import common

from ..language_plugin import PluginRegistry

_registry = PluginRegistry()

# Backward-compatible module-level exports (same shapes as old hardcoded dicts)
EXTENSION_TO_LANGUAGE = _registry.extension_to_language_enum()
LANGUAGE_MODULES = _registry.language_to_integration_module()


def get_patterns_for_language(
    language: Language | None,
    frameworks: list[str] = [],
) -> dict[IntegrationType, list[tuple[str, Confidence, str, SignalDirection]]]:
    """Get integration patterns for a specific language and active frameworks.

    Combines common patterns with language-specific base patterns and
    framework-specific patterns for any active frameworks.

    Args:
        language: The programming language, or None for common patterns only.
        frameworks: List of active framework names (e.g., ["FastAPI", "Django"]).
                    Framework-specific patterns for these frameworks
                    are included alongside base patterns.

    Returns:
        Dict mapping IntegrationType to list of
        (pattern, confidence, source, direction) tuples.
    """
    result: dict[IntegrationType, list[tuple[str, Confidence, str, SignalDirection]]] = {}

    for integration_type in IntegrationType:
        patterns: list[tuple[str, Confidence, str, SignalDirection]] = []

        # Add common patterns
        common_type_patterns = common.COMMON.patterns.get(integration_type, {})
        patterns.extend(
            (p, c, "common", d)
            for p, c, d in common_type_patterns.get(PatternKey.PATTERNS, [])
        )

        # Add language-specific base patterns
        if language is not None:
            lang_module = LANGUAGE_MODULES.get(language)
            if lang_module is not None:
                lang_source = language.value
                lang_type_patterns = lang_module.BASE_PATTERNS.get(integration_type, {})
                patterns.extend(
                    (p, c, lang_source, d)
                    for p, c, d in lang_type_patterns.get(PatternKey.PATTERNS, [])
                )

                # Add framework-specific patterns for active frameworks
                for framework in frameworks:
                    fw_patterns = lang_module.FRAMEWORK_PATTERNS.get(framework, {})
                    fw_type_patterns = fw_patterns.get(integration_type, {})
                    patterns.extend(
                        (p, c, framework, d)
                        for p, c, d in fw_type_patterns.get(PatternKey.PATTERNS, [])
                    )

        result[integration_type] = patterns

    return result


def get_import_patterns_for_framework(
    language: Language | None, framework: str
) -> tuple[str, ...]:
    """Retrieve import-gating patterns for a framework in a given language.

    Args:
        language: The programming language, or None.
        framework: The framework name (e.g., "Javalin", "Express").

    Returns:
        Tuple of regex patterns the file must match for the framework's patterns
        to apply. An empty tuple means the framework is ungated.
    """
    if language is None:
        return ()
    lang_module = LANGUAGE_MODULES.get(language)
    if lang_module is None:
        return ()
    return lang_module.FRAMEWORK_IMPORT_PATTERNS.get(framework, ())


def get_directory_patterns() -> dict[IntegrationType, list[str]]:
    """Get directory patterns for all integration types.

    Returns:
        Dict mapping IntegrationType to list of directory patterns.
    """
    return {
        integration_type: patterns.get(PatternKey.DIRECTORY_PATTERNS, [])
        for integration_type, patterns in common.COMMON.patterns.items()
    }


__all__ = [
    "Confidence",
    "IntegrationType",
    "Language",
    "SignalDirection",
    "EXTENSION_TO_LANGUAGE",
    "LANGUAGE_MODULES",
    "get_patterns_for_language",
    "get_import_patterns_for_framework",
    "get_directory_patterns",
    "common",
]
