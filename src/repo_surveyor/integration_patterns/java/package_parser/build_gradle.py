"""Parser for build.gradle and build.gradle.kts files."""

import re

from repo_surveyor.package_parsers.types import ParsedDependency

SOURCE = "build.gradle"

# Matches dependency declarations like:
#   implementation 'group:artifact:version'
#   implementation "group:artifact:version"
#   api("group:artifact:version")
#   testImplementation("group:artifact:version")
_DEP_PATTERN = re.compile(
    r"""(?:implementation|api|compile|runtime|testImplementation|"""
    r"""testCompile|compileOnly|runtimeOnly|annotationProcessor|kapt)"""
    r"""[\s(]+['"]([^'"]+)['"]""",
    re.MULTILINE,
)

# Matches group:artifact:version or group:artifact
_GAV = re.compile(r"^([^:]+):([^:]+)")


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from build.gradle or build.gradle.kts content."""
    names: list[str] = []

    for m in _DEP_PATTERN.finditer(content):
        coord = m.group(1)
        gav = _GAV.match(coord)
        if gav:
            names.append(gav.group(2).strip().lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
