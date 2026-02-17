"""Parser for pyproject.toml files."""

import re
import tomllib

from repo_surveyor.package_parsers.types import ParsedDependency

from .constants import PyprojectKeys

SOURCE = "pyproject.toml"

_PEP508_NAME = re.compile(r"^([A-Za-z0-9]([A-Za-z0-9._-]*[A-Za-z0-9])?)")


def _extract_name(spec: str) -> str:
    """Extract the package name from a PEP 508 dependency specifier."""
    m = _PEP508_NAME.match(spec.strip())
    return m.group(1).lower() if m else ""


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from pyproject.toml content."""
    try:
        data = tomllib.loads(content)
    except Exception:
        return []

    project = data.get(PyprojectKeys.PROJECT, {})

    # [project.dependencies] - PEP 621
    pep621_deps = [
        name
        for spec in project.get(PyprojectKeys.DEPENDENCIES, [])
        if (name := _extract_name(spec))
    ]

    # [project.optional-dependencies]
    optional_deps = [
        name
        for deps in project.get(PyprojectKeys.OPTIONAL_DEPENDENCIES, {}).values()
        for spec in deps
        if (name := _extract_name(spec))
    ]

    poetry = data.get(PyprojectKeys.TOOL, {}).get(PyprojectKeys.POETRY, {})

    # [tool.poetry.dependencies] + [tool.poetry.dev-dependencies]
    poetry_deps = [
        key.lower()
        for section in (PyprojectKeys.DEPENDENCIES, PyprojectKeys.DEV_DEPENDENCIES)
        for key in poetry.get(section, {})
    ]

    # [tool.poetry.group.*.dependencies]
    poetry_group_deps = [
        key.lower()
        for group in poetry.get(PyprojectKeys.GROUP, {}).values()
        for key in group.get(PyprojectKeys.DEPENDENCIES, {})
    ]

    names = pep621_deps + optional_deps + poetry_deps + poetry_group_deps

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
