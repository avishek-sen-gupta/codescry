"""Package parsers for structured config file parsing.

Dispatches to format-specific parsers and provides smart framework matching.
"""

from .types import ParsedDependency
from . import (
    build_gradle,
    cargo_toml,
    go_mod,
    package_json,
    pipfile,
    pom_xml,
    pyproject_toml,
    requirements_txt,
    setup_py,
)

_PARSERS: dict[str, callable] = {
    "pyproject.toml": pyproject_toml.parse,
    "requirements.txt": requirements_txt.parse,
    "Pipfile": pipfile.parse,
    "setup.py": setup_py.parse,
    "package.json": package_json.parse,
    "pom.xml": pom_xml.parse,
    "build.gradle": build_gradle.parse,
    "build.gradle.kts": build_gradle.parse,
    "go.mod": go_mod.parse,
    "Cargo.toml": cargo_toml.parse,
}


def parse_dependencies(filename: str, content: str) -> list[ParsedDependency]:
    """Parse dependencies from a config file.

    Dispatches to the appropriate parser based on filename.
    Returns an empty list for unrecognised filenames.
    """
    parser = _PARSERS.get(filename)
    if parser is None:
        return []
    return parser(content)


def _dep_matches_pattern(dep_name: str, pattern: str) -> bool:
    """Check whether a single dependency name matches a framework pattern.

    Matching rules (tried in order):
      1. Exact match
      2. Prefix-hyphen: dep starts with ``pattern + "-"``
      3. Path subsequence: pattern (possibly multi-segment) appears as a
         contiguous ``/``-delimited subsequence (e.g. ``"gin-gonic/gin"``
         matches ``"github.com/gin-gonic/gin"``)
      4. npm scoped: match ``@scope/name`` against ``scope`` or ``name``
    """
    if dep_name == pattern:
        return True
    if dep_name.startswith(pattern + "-"):
        return True
    # Contiguous path subsequence: /pattern/ within /dep/
    if "/" in dep_name:
        padded = "/" + dep_name + "/"
        if ("/" + pattern + "/") in padded:
            return True
    # npm scoped packages: @scope/name
    if dep_name.startswith("@") and "/" in dep_name:
        scope = dep_name[1:].split("/", 1)[0]  # "nestjs" from "@nestjs/core"
        name = dep_name.split("/", 1)[1]  # "core" from "@nestjs/core"
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
    frameworks: dict[str, None] = {}  # ordered set

    for dep in dependencies:
        for pattern, tech in patterns.items():
            if "frameworks" in tech and _dep_matches_pattern(dep.name, pattern):
                frameworks[tech["frameworks"]] = None

    return list(frameworks)
