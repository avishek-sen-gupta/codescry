"""Parser for conanfile.txt files."""

from repo_surveyor.package_parsers.types import ParsedDependency

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

        if stripped == "[requires]":
            in_requires = True
            continue

        if stripped.startswith("[") and stripped.endswith("]"):
            in_requires = False
            continue

        if in_requires and stripped and not stripped.startswith("#"):
            name = stripped.split("/")[0].strip().lower()
            if name:
                names.append(name)

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
