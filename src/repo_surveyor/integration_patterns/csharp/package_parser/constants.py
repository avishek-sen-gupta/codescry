"""Constants for C# package parsers."""


class CsprojElements:
    """XML element names for MSBuild PackageReference declarations."""

    PACKAGE_REFERENCE = "PackageReference"


class CsprojAttrs:
    """XML attribute names for PackageReference elements."""

    INCLUDE = "Include"
    INCLUDE_LOWER = "include"


class PackagesConfigElements:
    """XML element names for packages.config."""

    PACKAGE = "package"


class PackagesConfigAttrs:
    """XML attribute names for package elements."""

    ID = "id"
    ID_UPPER = "Id"
