"""Constants for C++ package parsers."""


class VcpkgKeys:
    """JSON key names for vcpkg.json."""

    DEPENDENCIES = "dependencies"
    NAME = "name"


class ConanfileMarkers:
    """Section and line markers for conanfile.txt INI-style format."""

    REQUIRES_SECTION = "[requires]"
    SECTION_OPEN = "["
    SECTION_CLOSE = "]"
    COMMENT = "#"
    VERSION_SEP = "/"
