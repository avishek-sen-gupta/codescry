"""Parser for composer.json files."""

import json

from repo_surveyor.package_parsers.types import ParsedDependency

from repo_surveyor.integration_patterns.php.package_parser.constants import (
    ComposerJsonKeys,
    PLATFORM_EXACT,
    PLATFORM_PREFIX,
)

SOURCE = "composer.json"


def _is_platform_package(name: str) -> bool:
    """Return True for platform requirements like 'php' and 'ext-*'."""
    return name == PLATFORM_EXACT or name.startswith(PLATFORM_PREFIX)


def parse(content: str) -> list[ParsedDependency]:
    """Parse dependencies from composer.json content."""
    try:
        data = json.loads(content)
    except Exception:
        return []

    names = [
        key.lower()
        for section in ComposerJsonKeys.ALL_SECTIONS
        for key in data.get(section, {})
        if not _is_platform_package(key.lower())
    ]

    return [ParsedDependency(name=n, source=SOURCE) for n in names]
