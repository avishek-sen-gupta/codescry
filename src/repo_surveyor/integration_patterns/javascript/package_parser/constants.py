"""Constants for JavaScript package parsers."""


class PackageJsonKeys:
    """JSON key names for package.json dependency sections."""

    DEPENDENCIES = "dependencies"
    DEV_DEPENDENCIES = "devDependencies"
    PEER_DEPENDENCIES = "peerDependencies"

    ALL_SECTIONS = (DEPENDENCIES, DEV_DEPENDENCIES, PEER_DEPENDENCIES)
