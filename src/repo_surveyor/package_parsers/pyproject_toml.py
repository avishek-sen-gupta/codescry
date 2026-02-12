"""Parser for pyproject.toml files."""

import re
import tomllib

from .types import ParsedDependency

SOURCE = "pyproject.toml"

_PEP508_NAME = re.compile(r"^([A-Za-z0-9]([A-Za-z0-9._-]*[A-Za-z0-9])?)")


def _extract_name(spec: str) -> str | None:
    """Extract the package name from a PEP 508 dependency specifier."""
    m = _PEP508_NAME.match(spec.strip())
    return m.group(1).lower() if m else None


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from pyproject.toml content."""
    try:
        data = tomllib.loads(content)
    except Exception:
        return []

    names: list[str] = []

    # [project.dependencies] - PEP 621
    for spec in data.get("project", {}).get("dependencies", []):
        name = _extract_name(spec)
        if name:
            names.append(name)

    # [project.optional-dependencies]
    for deps in data.get("project", {}).get("optional-dependencies", {}).values():
        for spec in deps:
            name = _extract_name(spec)
            if name:
                names.append(name)

    # [tool.poetry.dependencies]
    for key in data.get("tool", {}).get("poetry", {}).get("dependencies", {}):
        names.append(key.lower())

    # [tool.poetry.dev-dependencies]
    for key in data.get("tool", {}).get("poetry", {}).get("dev-dependencies", {}):
        names.append(key.lower())

    # [tool.poetry.group.*.dependencies]
    for group in data.get("tool", {}).get("poetry", {}).get("group", {}).values():
        for key in group.get("dependencies", {}):
            names.append(key.lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
