"""Parser for go.mod files."""

import re

from .types import ParsedDependency

SOURCE = "go.mod"

# Matches module paths inside require blocks or single-line require
_REQUIRE_BLOCK = re.compile(r"require\s*\((.*?)\)", re.DOTALL)
_REQUIRE_SINGLE = re.compile(r"^require\s+(\S+)", re.MULTILINE)
_MODULE_LINE = re.compile(r"^\s*(\S+)\s+\S+", re.MULTILINE)


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from go.mod content."""
    modules: list[str] = []

    # Multi-line require blocks
    for block in _REQUIRE_BLOCK.finditer(content):
        for line_match in _MODULE_LINE.finditer(block.group(1)):
            path = line_match.group(1)
            if not path.startswith("//"):
                modules.append(path.lower())

    # Single-line require statements
    for m in _REQUIRE_SINGLE.finditer(content):
        modules.append(m.group(1).lower())

    return [ParsedDependency(name=mod, source=SOURCE) for mod in modules]
