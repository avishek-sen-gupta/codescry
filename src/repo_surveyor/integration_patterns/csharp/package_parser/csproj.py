"""Parser for .csproj files (MSBuild project files with NuGet PackageReference)."""

import xml.etree.ElementTree as ET

from repo_surveyor.package_parsers.types import ParsedDependency

from repo_surveyor.integration_patterns.csharp.package_parser.constants import (
    CsprojAttrs,
    CsprojElements,
)

SOURCE = ".csproj"

_MSBUILD_NS = "{http://schemas.microsoft.com/developer/msbuild/2003}"


def parse(content: str) -> list[ParsedDependency]:
    """Parse NuGet dependencies from .csproj content.

    Extracts the Include attribute from <PackageReference> elements,
    handling both namespaced (old-style) and non-namespaced (SDK-style) projects.
    """
    try:
        root = ET.fromstring(content)
    except ET.ParseError:
        return []

    names: list[str] = []

    for ns in (_MSBUILD_NS, ""):
        for ref in root.iter(f"{ns}{CsprojElements.PACKAGE_REFERENCE}"):
            include = ref.get(CsprojAttrs.INCLUDE) or ref.get(CsprojAttrs.INCLUDE_LOWER)
            if include:
                names.append(include.strip().lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
