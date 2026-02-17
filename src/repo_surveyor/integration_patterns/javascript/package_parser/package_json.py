"""Parser for package.json files."""

import json

from repo_surveyor.package_parsers.types import ParsedDependency

from .constants import PackageJsonKeys

SOURCE = "package.json"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from package.json content."""
    try:
        data = json.loads(content)
    except Exception:
        return []

    names = [
        key.lower()
        for section in PackageJsonKeys.ALL_SECTIONS
        for key in data.get(section, {})
    ]

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
