"""Parser for Pipfile files."""

import tomllib

from repo_surveyor.package_parsers.types import ParsedDependency

from .constants import PipfileKeys

SOURCE = "Pipfile"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from Pipfile content."""
    try:
        data = tomllib.loads(content)
    except Exception:
        return []

    names = [
        key.lower()
        for section in (PipfileKeys.PACKAGES, PipfileKeys.DEV_PACKAGES)
        for key in data.get(section, {})
    ]

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
