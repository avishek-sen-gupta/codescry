"""Constants for PHP package parsers."""


class ComposerJsonKeys:
    """JSON key names for composer.json dependency sections."""

    REQUIRE = "require"
    REQUIRE_DEV = "require-dev"

    ALL_SECTIONS = (REQUIRE, REQUIRE_DEV)


PLATFORM_EXACT = "php"
PLATFORM_PREFIX = "ext-"
