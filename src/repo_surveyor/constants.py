"""Centralised string constants used across the codebase."""

from enum import StrEnum


class TechCategory(StrEnum):
    """Dict keys for the four tech categories that flow through detectors → surveyor → report."""

    LANGUAGES = "languages"
    PACKAGE_MANAGERS = "package_managers"
    FRAMEWORKS = "frameworks"
    INFRASTRUCTURE = "infrastructure"


class MarkerKey:
    """Dict keys for marker result dicts produced by detectors.py."""

    DIRECTORY = "directory"
    MARKER_FILE = "marker_file"


class TechLabel:
    """Neo4j node labels for technology nodes."""

    LANGUAGE = "Language"
    PACKAGE_MANAGER = "PackageManager"
    FRAMEWORK = "Framework"
    INFRASTRUCTURE = "Infrastructure"


class TechRelType:
    """Neo4j relationship types for technology edges."""

    USES_LANGUAGE = "USES_LANGUAGE"
    USES_PACKAGE_MANAGER = "USES_PACKAGE_MANAGER"
    USES_FRAMEWORK = "USES_FRAMEWORK"
    USES_INFRASTRUCTURE = "USES_INFRASTRUCTURE"


class IntegrationLabel:
    """Neo4j node labels for integration nodes."""

    INTEGRATION_TYPE = "IntegrationType"
    INTEGRATION_SIGNAL = "IntegrationSignal"
    UNRESOLVED_INTEGRATION = "UnresolvedIntegration"


class IntegrationRelType:
    """Neo4j relationship types for integration edges."""

    HAS_INTEGRATION = "HAS_INTEGRATION"
    OF_TYPE = "OF_TYPE"
    HAS_UNRESOLVED_INTEGRATION = "HAS_UNRESOLVED_INTEGRATION"
