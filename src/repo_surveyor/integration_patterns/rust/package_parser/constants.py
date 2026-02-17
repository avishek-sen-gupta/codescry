"""Constants for Rust package parsers."""


class CargoKeys:
    """TOML section names for Cargo.toml dependency tables."""

    DEPENDENCIES = "dependencies"
    DEV_DEPENDENCIES = "dev-dependencies"
    BUILD_DEPENDENCIES = "build-dependencies"

    ALL_SECTIONS = (DEPENDENCIES, DEV_DEPENDENCIES, BUILD_DEPENDENCIES)
