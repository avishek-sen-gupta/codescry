"""Parser for pom.xml files."""

import xml.etree.ElementTree as ET

from repo_surveyor.package_parsers.types import ParsedDependency

SOURCE = "pom.xml"

_MAVEN_NS = "{http://maven.apache.org/POM/4.0.0}"


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from pom.xml content.

    Extracts artifactId from <dependency> elements, handling both
    namespaced and non-namespaced POM files.
    """
    try:
        root = ET.fromstring(content)
    except ET.ParseError:
        return []

    # Try both namespaced and non-namespaced paths
    names = [
        artifact_id_el.text.strip().lower()
        for ns in (_MAVEN_NS, "")
        for dep in root.iter(f"{ns}dependency")
        for artifact_id_el in [dep.find(f"{ns}artifactId")]
        if artifact_id_el is not None and artifact_id_el.text
    ]

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
