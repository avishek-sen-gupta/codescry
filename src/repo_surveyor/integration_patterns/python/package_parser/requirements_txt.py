"""Parser for requirements.txt files."""

import re

from repo_surveyor.package_parsers.types import ParsedDependency

from .constants import RequirementsTxtMarkers

SOURCE = "requirements.txt"

_PEP508_NAME = re.compile(r"^([A-Za-z0-9]([A-Za-z0-9._-]*[A-Za-z0-9])?)")


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from requirements.txt content."""
    names: list[str] = []

    for line in content.splitlines():
        line = line.strip()
        # Skip empty lines, comments, and option lines
        if (
            not line
            or line.startswith(RequirementsTxtMarkers.COMMENT)
            or line.startswith(RequirementsTxtMarkers.OPTION)
        ):
            continue
        m = _PEP508_NAME.match(line)
        if m:
            names.append(m.group(1).lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
