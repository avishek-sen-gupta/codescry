"""Parser for conanfile.txt files."""

from repo_surveyor.package_parsers.types import ParsedDependency

from .constants import ConanfileMarkers

SOURCE = "conanfile.txt"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from conanfile.txt content.

    Reads the [requires] section and extracts package names
    (the part before the '/' version separator).
    """
    names: list[str] = []
    in_requires = False

    for line in content.splitlines():
        stripped = line.strip()

        if stripped == ConanfileMarkers.REQUIRES_SECTION:
            in_requires = True
            continue

        if stripped.startswith(ConanfileMarkers.SECTION_OPEN) and stripped.endswith(
            ConanfileMarkers.SECTION_CLOSE
        ):
            in_requires = False
            continue

        if (
            in_requires
            and stripped
            and not stripped.startswith(ConanfileMarkers.COMMENT)
        ):
            name = stripped.split(ConanfileMarkers.VERSION_SEP)[0].strip().lower()
            if name:
                names.append(name)

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
