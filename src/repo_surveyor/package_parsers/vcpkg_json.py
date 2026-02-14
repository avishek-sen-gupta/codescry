"""Parser for vcpkg.json files."""

import json

from .types import ParsedDependency

SOURCE = "vcpkg.json"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from vcpkg.json content.

    Dependencies can be strings ("boost-asio") or objects
    ({"name": "qt5-base", "features": [...]}).
    """
    try:
        data = json.loads(content)
    except Exception:
        return []

    names: list[str] = []

    for dep in data.get("dependencies", []):
        if isinstance(dep, str):
            names.append(dep.lower())
        elif isinstance(dep, dict) and "name" in dep:
            names.append(dep["name"].lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
