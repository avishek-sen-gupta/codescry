"""Parser for vcpkg.json files."""

import json

from repo_surveyor.package_parsers.types import ParsedDependency

from repo_surveyor.integration_patterns.cpp.package_parser.constants import VcpkgKeys

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

    for dep in data.get(VcpkgKeys.DEPENDENCIES, []):
        if isinstance(dep, str):
            names.append(dep.lower())
        elif isinstance(dep, dict) and VcpkgKeys.NAME in dep:
            names.append(dep[VcpkgKeys.NAME].lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
