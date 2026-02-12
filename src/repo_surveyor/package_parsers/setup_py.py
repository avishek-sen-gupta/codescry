"""Parser for setup.py files."""

import re

from .types import ParsedDependency

SOURCE = "setup.py"

_PEP508_NAME = re.compile(r"^([A-Za-z0-9]([A-Za-z0-9._-]*[A-Za-z0-9])?)")

# Match install_requires=[...] or install_requires = [...] with contents
_INSTALL_REQUIRES = re.compile(
    r"install_requires\s*=\s*\[([^\]]*)\]", re.DOTALL
)

_STRING_LITERAL = re.compile(r"""['"]([^'"]+)['"]""")


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from setup.py content."""
    names: list[str] = []

    m = _INSTALL_REQUIRES.search(content)
    if m:
        for string_match in _STRING_LITERAL.finditer(m.group(1)):
            spec = string_match.group(1)
            name_match = _PEP508_NAME.match(spec.strip())
            if name_match:
                names.append(name_match.group(1).lower())

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
