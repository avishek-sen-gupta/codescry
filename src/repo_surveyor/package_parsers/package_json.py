"""Parser for package.json files."""

import json

from .types import ParsedDependency

SOURCE = "package.json"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from package.json content."""
    try:
        data = json.loads(content)
    except Exception:
        return []

    names: list[str] = []

    for section in ("dependencies", "devDependencies", "peerDependencies"):
        for key in data.get(section, {}):
            names.append(key.lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
