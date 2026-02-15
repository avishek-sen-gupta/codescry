"""Parser for Cargo.toml files."""

import tomllib

from .types import ParsedDependency

SOURCE = "Cargo.toml"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from Cargo.toml content."""
    try:
        data = tomllib.loads(content)
    except Exception:
        return []

    names = [
        key.lower()
        for section in ("dependencies", "dev-dependencies", "build-dependencies")
        for key in data.get(section, {})
    ]

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
