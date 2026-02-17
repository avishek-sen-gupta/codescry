"""Constants for Python package parsers."""


class PyprojectKeys:
    """TOML key names for pyproject.toml sections."""

    PROJECT = "project"
    DEPENDENCIES = "dependencies"
    OPTIONAL_DEPENDENCIES = "optional-dependencies"
    DEV_DEPENDENCIES = "dev-dependencies"
    TOOL = "tool"
    POETRY = "poetry"
    GROUP = "group"


class PipfileKeys:
    """TOML section names for Pipfile."""

    PACKAGES = "packages"
    DEV_PACKAGES = "dev-packages"


class RequirementsTxtMarkers:
    """Line prefix markers for requirements.txt."""

    COMMENT = "#"
    OPTION = "-"
