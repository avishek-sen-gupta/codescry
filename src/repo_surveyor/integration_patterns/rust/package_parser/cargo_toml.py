"""Parser for Cargo.toml files."""

import tomllib

from repo_surveyor.package_parsers.types import ParsedDependency

from repo_surveyor.integration_patterns.rust.package_parser.constants import CargoKeys

SOURCE = "Cargo.toml"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from Cargo.toml content."""
    try:
        data = tomllib.loads(content)
    except Exception:
        return []

    names = [
        key.lower()
        for section in CargoKeys.ALL_SECTIONS
        for key in data.get(section, {})
    ]

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
