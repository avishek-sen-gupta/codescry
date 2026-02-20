"""Parser for packages.config files (legacy NuGet package format)."""

import xml.etree.ElementTree as ET

from repo_surveyor.package_parsers.types import ParsedDependency

from repo_surveyor.integration_patterns.csharp.package_parser.constants import (
    PackagesConfigAttrs,
    PackagesConfigElements,
)

SOURCE = "packages.config"


def parse(content: str) -> list[ParsedDependency]:
    """Parse NuGet dependencies from packages.config content.

    Extracts the id attribute from <package> elements.
    """
    try:
        root = ET.fromstring(content)
    except ET.ParseError:
        return []

    names: list[str] = []

    for pkg in root.iter(PackagesConfigElements.PACKAGE):
        pkg_id = pkg.get(PackagesConfigAttrs.ID) or pkg.get(
            PackagesConfigAttrs.ID_UPPER
        )
        if pkg_id:
            names.append(pkg_id.strip().lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
