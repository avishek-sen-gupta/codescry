"""Package parsers for structured config file parsing.

Dispatches to format-specific parsers and provides smart framework matching.
"""

from ..core.constants import TechCategory
from .constants import DepSeparators
from .types import ParsedDependency
from ..integration_patterns.python.package_parser import (
    pyproject_toml,
    requirements_txt,
    pipfile,
    setup_py,
)
from ..integration_patterns.javascript.package_parser import package_json
from ..integration_patterns.java.package_parser import pom_xml, build_gradle
from ..integration_patterns.go.package_parser import go_mod
from ..integration_patterns.rust.package_parser import cargo_toml
from ..integration_patterns.csharp.package_parser import csproj, packages_config
from ..integration_patterns.cpp.package_parser import vcpkg_json, conanfile_txt
from ..integration_patterns.php.package_parser import composer_json
from ..integration_patterns.scala.package_parser import build_sbt

_PARSERS: dict[str, callable] = {
    pyproject_toml.SOURCE: pyproject_toml.parse,
    requirements_txt.SOURCE: requirements_txt.parse,
    pipfile.SOURCE: pipfile.parse,
    setup_py.SOURCE: setup_py.parse,
    package_json.SOURCE: package_json.parse,
    pom_xml.SOURCE: pom_xml.parse,
    build_gradle.SOURCE: build_gradle.parse,
    build_gradle.SOURCE_KTS: build_gradle.parse,
    go_mod.SOURCE: go_mod.parse,
    cargo_toml.SOURCE: cargo_toml.parse,
    packages_config.SOURCE: packages_config.parse,
    vcpkg_json.SOURCE: vcpkg_json.parse,
    conanfile_txt.SOURCE: conanfile_txt.parse,
    composer_json.SOURCE: composer_json.parse,
    build_sbt.SOURCE: build_sbt.parse,
}

# Extension-based fallback for files with variable names (e.g. MyApp.csproj)
_EXT_PARSERS: dict[str, callable] = {
    csproj.SOURCE: csproj.parse,
}


def parse_dependencies(filename: str, content: str) -> list[ParsedDependency]:
    """Parse dependencies from a config file.

    Dispatches to the appropriate parser based on exact filename first,
    then falls back to file extension matching.
    Returns an empty list for unrecognised filenames.
    """
    parser = _PARSERS.get(filename)
    if parser is not None:
        return parser(content)

    # Extension-based fallback
    for ext, ext_parser in _EXT_PARSERS.items():
        if filename.endswith(ext):
            return ext_parser(content)

    return []


def _dep_matches_pattern(dep_name: str, pattern: str) -> bool:
    """Check whether a single dependency name matches a framework pattern.

    Matching rules (tried in order):
      1. Exact match
      2. Prefix-separator: dep starts with ``pattern + "-"`` or ``pattern + "."``
      3. Path subsequence: pattern (possibly multi-segment) appears as a
         contiguous ``/``-delimited subsequence (e.g. ``"gin-gonic/gin"``
         matches ``"github.com/gin-gonic/gin"``)
      4. npm scoped: match ``@scope/name`` against ``scope`` or ``name``
    """
    if dep_name == pattern:
        return True
    if dep_name.startswith(pattern + DepSeparators.HYPHEN) or dep_name.startswith(
        pattern + DepSeparators.DOT
    ):
        return True
    # Contiguous path subsequence: /pattern/ within /dep/
    if DepSeparators.PATH in dep_name:
        padded = DepSeparators.PATH + dep_name + DepSeparators.PATH
        if (DepSeparators.PATH + pattern + DepSeparators.PATH) in padded:
            return True
    # npm scoped packages: @scope/name
    if dep_name.startswith(DepSeparators.NPM_SCOPE) and DepSeparators.PATH in dep_name:
        scope = dep_name[1:].split(DepSeparators.PATH, 1)[0]
        name = dep_name.split(DepSeparators.PATH, 1)[1]
        if scope == pattern or name == pattern:
            return True
    return False


def match_frameworks(
    dependencies: list[ParsedDependency],
    patterns: dict[str, dict[str, str]],
) -> list[str]:
    """Match parsed dependencies against framework patterns.

    Returns a deduplicated list of framework names.
    """
    seen: set[str] = set()
    frameworks: list[str] = []

    for dep in dependencies:
        for pattern, tech in patterns.items():
            if TechCategory.FRAMEWORKS in tech and _dep_matches_pattern(
                dep.name, pattern
            ):
                name = tech[TechCategory.FRAMEWORKS]
                if name not in seen:
                    seen.add(name)
                    frameworks.append(name)

    return frameworks
