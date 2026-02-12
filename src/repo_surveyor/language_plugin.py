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

if TYPE_CHECKING:
    from .integration_patterns.types import Language


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
            extensions = tuple(lang_data.get("extensions", []))

            # Build indicator file configs
            indicator_files: list[IndicatorFileConfig] = []
            for filename, file_cfg in lang_data.get("indicator_files", {}).items():
                additional = file_cfg.get("additional_languages", [])
                all_languages = (lang_name, *additional)
                indicator_files.append(
                    IndicatorFileConfig(
                        filename=filename,
                        languages=all_languages,
                        package_managers=tuple(file_cfg.get("package_managers", [])),
                        parser_module_name=file_cfg.get("parser", ""),
                    )
                )

            # Build glob indicator configs
            glob_indicators: list[GlobIndicatorConfig] = []
            for pattern, glob_cfg in lang_data.get("glob_indicators", {}).items():
                glob_indicators.append(
                    GlobIndicatorConfig(
                        pattern=pattern,
                        languages=(lang_name,),
                        package_managers=tuple(glob_cfg.get("package_managers", [])),
                        frameworks=tuple(glob_cfg.get("frameworks", [])),
                        parser_module_name=glob_cfg.get("parser", ""),
                    )
                )

            pm_indicators = lang_data.get("package_manager_indicators", {})
            framework_patterns = lang_data.get("framework_patterns", {})
            integration_module = lang_data.get("integration_module", "")

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
            shared_ref = lang_data.get("shared_framework_patterns")
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
        for name, cfg in infra_raw.items():
            self._infra[name] = InfrastructureConfig(
                name=name,
                indicator_files=tuple(cfg.get("indicator_files", [])),
                glob_indicators=tuple(cfg.get("glob_indicators", [])),
                yaml_content_markers=tuple(cfg.get("yaml_content_markers", [])),
            )

    # --- Backward-compatible query APIs ---

    def indicator_files(self) -> dict[str, dict[str, list[str]]]:
        """Produce the same shape as the old INDICATOR_FILES dict in detectors.py."""
        result: dict[str, dict[str, list[str]]] = {}

        # Language indicator files
        for plugin in self._plugins.values():
            for ind in plugin.indicator_files:
                entry: dict[str, list[str]] = {}
                if ind.languages:
                    entry["languages"] = list(ind.languages)
                if ind.package_managers:
                    entry["package_managers"] = list(ind.package_managers)
                result[ind.filename] = entry

            # Package manager indicators (no language, just PM)
            for filename, pm_name in plugin.package_manager_indicators.items():
                result[filename] = {"package_managers": [pm_name]}

        # Infrastructure indicator files
        for infra in self._infra.values():
            for filename in infra.indicator_files:
                result[filename] = {"infrastructure": [infra.name]}

        return result

    def extension_languages(self) -> dict[str, str]:
        """Produce the same shape as the old EXTENSION_LANGUAGES dict."""
        result: dict[str, str] = {}
        for plugin in self._plugins.values():
            for ext in plugin.extensions:
                result[ext] = plugin.name
        return result

    def all_framework_patterns(self) -> dict[str, dict[str, str]]:
        """Produce the same shape as the old FRAMEWORK_PATTERNS dict.

        Merges framework_patterns from all languages into a single flat dict.
        """
        result: dict[str, dict[str, str]] = {}
        for plugin in self._plugins.values():
            for dep_pattern, framework_name in plugin.framework_patterns.items():
                result[dep_pattern] = {"frameworks": framework_name}
        return result

    def glob_patterns(self) -> dict[str, dict[str, list[str]]]:
        """Produce the same shape as the old GLOB_PATTERNS dict."""
        result: dict[str, dict[str, list[str]]] = {}

        # Language glob indicators
        for plugin in self._plugins.values():
            for glob_ind in plugin.glob_indicators:
                entry: dict[str, list[str]] = {}
                if glob_ind.languages:
                    entry["languages"] = list(glob_ind.languages)
                if glob_ind.package_managers:
                    entry["package_managers"] = list(glob_ind.package_managers)
                if glob_ind.frameworks:
                    entry["frameworks"] = list(glob_ind.frameworks)
                result[glob_ind.pattern] = entry

        # Infrastructure glob indicators
        for infra in self._infra.values():
            for pattern in infra.glob_indicators:
                result[pattern] = {"infrastructure": [infra.name]}

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
        from .integration_patterns.types import Language as Lang

        result: dict[str, Lang] = {}
        for plugin in self._plugins.values():
            lang_enum = Lang.from_name(plugin.name)
            if lang_enum is not None:
                for ext in plugin.extensions:
                    result[ext] = lang_enum
        return result

    def language_to_integration_module(self) -> dict[Language, Any]:
        """Produce the same shape as LANGUAGE_MODULES in integration_patterns.

        Dynamically imports integration pattern modules listed in languages.json.
        """
        from .integration_patterns.types import Language as Lang

        result: dict[str, Lang] = {}
        for plugin in self._plugins.values():
            if not plugin.integration_module_name:
                continue
            lang_enum = Lang.from_name(plugin.name)
            if lang_enum is None:
                continue
            module = importlib.import_module(
                f".integration_patterns.{plugin.integration_module_name}",
                package="repo_surveyor",
            )
            result[lang_enum] = module
        return result

    # --- Direct plugin access ---

    def get_plugin(self, name: str) -> LanguagePlugin | None:
        return self._plugins.get(name)

    def all_plugins(self) -> list[LanguagePlugin]:
        return list(self._plugins.values())
