"""Parser for Pipfile files."""

import tomllib

from repo_surveyor.package_parsers.types import ParsedDependency

SOURCE = "Pipfile"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from Pipfile content."""
    try:
        data = tomllib.loads(content)
    except Exception:
        return []

    names: list[str] = []

    for key in data.get("packages", {}):
        names.append(key.lower())

    for key in data.get("dev-packages", {}):
        names.append(key.lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
