"""Plugin registry for declarative language support.

Loads language and infrastructure definitions from languages.json and
provides backward-compatible lookup tables for detectors.py and
integration_patterns/__init__.py.
"""

from __future__ import annotations

import importlib
import json
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

from repo_surveyor.core.constants import TechCategory

if TYPE_CHECKING:
    from repo_surveyor.integration_patterns.types import Language

# JSON config keys (used only within this module)
_EXTENSIONS = "extensions"
_INDICATOR_FILES = "indicator_files"
_GLOB_INDICATORS = "glob_indicators"
_PACKAGE_MANAGERS = "package_managers"
_FRAMEWORKS = "frameworks"
_PARSER = "parser"
_ADDITIONAL_LANGUAGES = "additional_languages"
_PACKAGE_MANAGER_INDICATORS = "package_manager_indicators"
_FRAMEWORK_PATTERNS = "framework_patterns"
_INTEGRATION_MODULE = "integration_module"
_SHARED_FRAMEWORK_PATTERNS = "shared_framework_patterns"
_YAML_CONTENT_MARKERS = "yaml_content_markers"


@dataclass(frozen=True)
class IndicatorFileConfig:
    """Configuration for an indicator file (e.g. pyproject.toml, pom.xml)."""

    filename: str
    languages: tuple[str, ...]
    package_managers: tuple[str, ...]
    parser_module_name: str


@dataclass(frozen=True)
class GlobIndicatorConfig:
    """Configuration for a glob-based indicator (e.g. *.csproj)."""

    pattern: str
    languages: tuple[str, ...]
    package_managers: tuple[str, ...]
    frameworks: tuple[str, ...]
    parser_module_name: str


@dataclass(frozen=True)
class InfrastructureConfig:
    """Configuration for an infrastructure technology."""

    name: str
    indicator_files: tuple[str, ...]
    glob_indicators: tuple[str, ...]
    yaml_content_markers: tuple[str, ...]


@dataclass(frozen=True)
class LanguagePlugin:
    """Declarative language definition loaded from JSON."""

    name: str
    extensions: tuple[str, ...]
    indicator_files: tuple[IndicatorFileConfig, ...]
    glob_indicators: tuple[GlobIndicatorConfig, ...]
    package_manager_indicators: dict[str, str]
    framework_patterns: dict[str, str]
    integration_module_name: str


class PluginRegistry:
    """Loads languages.json and builds backward-compatible lookup tables."""

    def __init__(self, config_path: Path | None = None) -> None:
        if config_path is None:
            config_path = Path(__file__).parent / "languages.json"

        with open(config_path, encoding="utf-8") as f:
            raw = json.load(f)

        self._plugins: dict[str, LanguagePlugin] = {}
        self._infra: dict[str, InfrastructureConfig] = {}

        self._load_languages(raw.get("languages", {}))
        self._load_infrastructure(raw.get("infrastructure", {}))

    def _load_languages(self, languages_raw: dict[str, Any]) -> None:
        # First pass: build all plugins with their own framework_patterns
        for lang_name, lang_data in languages_raw.items():
            extensions = tuple(lang_data.get(_EXTENSIONS, []))

            # Build indicator file configs
            indicator_files = [
                IndicatorFileConfig(
                    filename=filename,
                    languages=(lang_name, *file_cfg.get(_ADDITIONAL_LANGUAGES, [])),
                    package_managers=tuple(file_cfg.get(_PACKAGE_MANAGERS, [])),
                    parser_module_name=file_cfg.get(_PARSER, ""),
                )
                for filename, file_cfg in lang_data.get(_INDICATOR_FILES, {}).items()
            ]

            # Build glob indicator configs
            glob_indicators = [
                GlobIndicatorConfig(
                    pattern=pattern,
                    languages=(lang_name,),
                    package_managers=tuple(glob_cfg.get(_PACKAGE_MANAGERS, [])),
                    frameworks=tuple(glob_cfg.get(_FRAMEWORKS, [])),
                    parser_module_name=glob_cfg.get(_PARSER, ""),
                )
                for pattern, glob_cfg in lang_data.get(_GLOB_INDICATORS, {}).items()
            ]

            pm_indicators = lang_data.get(_PACKAGE_MANAGER_INDICATORS, {})
            framework_patterns = lang_data.get(_FRAMEWORK_PATTERNS, {})
            integration_module = lang_data.get(_INTEGRATION_MODULE, "")

            self._plugins[lang_name] = LanguagePlugin(
                name=lang_name,
                extensions=extensions,
                indicator_files=tuple(indicator_files),
                glob_indicators=tuple(glob_indicators),
                package_manager_indicators=dict(pm_indicators),
                framework_patterns=dict(framework_patterns),
                integration_module_name=integration_module,
            )

        # Second pass: resolve shared_framework_patterns
        for lang_name, lang_data in languages_raw.items():
            shared_ref = lang_data.get(_SHARED_FRAMEWORK_PATTERNS)
            if shared_ref and shared_ref in self._plugins:
                source_patterns = self._plugins[shared_ref].framework_patterns
                current = self._plugins[lang_name]
                merged = {**source_patterns, **current.framework_patterns}
                self._plugins[lang_name] = LanguagePlugin(
                    name=current.name,
                    extensions=current.extensions,
                    indicator_files=current.indicator_files,
                    glob_indicators=current.glob_indicators,
                    package_manager_indicators=current.package_manager_indicators,
                    framework_patterns=merged,
                    integration_module_name=current.integration_module_name,
                )

    def _load_infrastructure(self, infra_raw: dict[str, Any]) -> None:
        self._infra = {
            name: InfrastructureConfig(
                name=name,
                indicator_files=tuple(cfg.get(_INDICATOR_FILES, [])),
                glob_indicators=tuple(cfg.get(_GLOB_INDICATORS, [])),
                yaml_content_markers=tuple(cfg.get(_YAML_CONTENT_MARKERS, [])),
            )
            for name, cfg in infra_raw.items()
        }

    # --- Backward-compatible query APIs ---

    def indicator_files(self) -> dict[str, dict[str, list[str]]]:
        """Produce the same shape as the old INDICATOR_FILES dict in detectors.py."""

        def _build_indicator_entry(ind: IndicatorFileConfig) -> dict[str, list[str]]:
            return {
                **(
                    {TechCategory.LANGUAGES: list(ind.languages)}
                    if ind.languages
                    else {}
                ),
                **(
                    {TechCategory.PACKAGE_MANAGERS: list(ind.package_managers)}
                    if ind.package_managers
                    else {}
                ),
            }

        # Language indicator files
        result: dict[str, dict[str, list[str]]] = {
            ind.filename: _build_indicator_entry(ind)
            for plugin in self._plugins.values()
            for ind in plugin.indicator_files
        }

        # Package manager indicators (no language, just PM)
        result |= {
            filename: {TechCategory.PACKAGE_MANAGERS: [pm_name]}
            for plugin in self._plugins.values()
            for filename, pm_name in plugin.package_manager_indicators.items()
        }

        # Infrastructure indicator files
        result |= {
            filename: {TechCategory.INFRASTRUCTURE: [infra.name]}
            for infra in self._infra.values()
            for filename in infra.indicator_files
        }

        return result

    def extension_languages(self) -> dict[str, str]:
        """Produce the same shape as the old EXTENSION_LANGUAGES dict."""
        return {
            ext: plugin.name
            for plugin in self._plugins.values()
            for ext in plugin.extensions
        }

    def all_framework_patterns(self) -> dict[str, dict[str, str]]:
        """Produce the same shape as the old FRAMEWORK_PATTERNS dict.

        Merges framework_patterns from all languages into a single flat dict.
        """
        return {
            dep_pattern: {TechCategory.FRAMEWORKS: framework_name}
            for plugin in self._plugins.values()
            for dep_pattern, framework_name in plugin.framework_patterns.items()
        }

    def glob_patterns(self) -> dict[str, dict[str, list[str]]]:
        """Produce the same shape as the old GLOB_PATTERNS dict."""

        def _build_glob_entry(glob_ind: GlobIndicatorConfig) -> dict[str, list[str]]:
            return {
                **(
                    {TechCategory.LANGUAGES: list(glob_ind.languages)}
                    if glob_ind.languages
                    else {}
                ),
                **(
                    {TechCategory.PACKAGE_MANAGERS: list(glob_ind.package_managers)}
                    if glob_ind.package_managers
                    else {}
                ),
                **(
                    {TechCategory.FRAMEWORKS: list(glob_ind.frameworks)}
                    if glob_ind.frameworks
                    else {}
                ),
            }

        # Language glob indicators
        result: dict[str, dict[str, list[str]]] = {
            glob_ind.pattern: _build_glob_entry(glob_ind)
            for plugin in self._plugins.values()
            for glob_ind in plugin.glob_indicators
        }

        # Infrastructure glob indicators
        result |= {
            pattern: {TechCategory.INFRASTRUCTURE: [infra.name]}
            for infra in self._infra.values()
            for pattern in infra.glob_indicators
        }

        return result

    def k8s_markers(self) -> list[str]:
        """Produce the same shape as the old K8S_MARKERS list."""
        k8s = self._infra.get("Kubernetes")
        if k8s is None:
            return []
        return list(k8s.yaml_content_markers)

    # --- Integration pattern layer ---

    def extension_to_language_enum(self) -> dict[str, Language]:
        """Produce the same shape as EXTENSION_TO_LANGUAGE in integration_patterns."""
        from repo_surveyor.integration_patterns.types import Language as Lang

        return {
            ext: lang_enum
            for plugin in self._plugins.values()
            if (lang_enum := Lang.from_name(plugin.name)) is not None
            for ext in plugin.extensions
        }

    def language_to_integration_module(self) -> dict[Language, Any]:
        """Produce the same shape as LANGUAGE_MODULES in integration_patterns.

        Dynamically imports integration pattern modules listed in languages.json.
        """
        from repo_surveyor.integration_patterns.types import Language as Lang

        return {
            lang_enum: importlib.import_module(
                f".integration_patterns.{plugin.integration_module_name}",
                package="repo_surveyor",
            )
            for plugin in self._plugins.values()
            if plugin.integration_module_name
            and (lang_enum := Lang.from_name(plugin.name)) is not None
        }

    # --- Direct plugin access ---

    def get_plugin(self, name: str) -> LanguagePlugin | None:
        return self._plugins.get(name)

    def all_plugins(self) -> list[LanguagePlugin]:
        return list(self._plugins.values())
