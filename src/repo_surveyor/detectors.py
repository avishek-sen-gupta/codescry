"""Detection logic for various technologies."""

import os
from pathlib import Path

from .language_plugin import PluginRegistry

_registry = PluginRegistry()

# Backward-compatible module-level exports (same variable names, same shapes)
INDICATOR_FILES = _registry.indicator_files()
EXTENSION_LANGUAGES = _registry.extension_languages()
FRAMEWORK_PATTERNS = _registry.all_framework_patterns()
GLOB_PATTERNS = _registry.glob_patterns()
K8S_MARKERS = _registry.k8s_markers()


def detect_from_indicator_files(repo_path: Path) -> dict[str, set[str]]:
    """Detect technologies from indicator files in root only."""
    results: dict[str, set[str]] = {
        "languages": set(),
        "package_managers": set(),
        "frameworks": set(),
        "infrastructure": set(),
    }

    for filename, techs in INDICATOR_FILES.items():
        if (repo_path / filename).exists():
            for category, items in techs.items():
                results[category].update(items)

    return results


def detect_indicator_files_with_directories(
    repo_path: Path,
) -> list[dict]:
    """Detect indicator files and associate them with their directories.

    Returns a list of dicts with directory, marker_file, and detected technologies.
    """
    results: list[dict] = []

    # Skip directories that shouldn't be scanned
    skip_dirs = {
        ".git",
        "node_modules",
        "__pycache__",
        ".venv",
        "venv",
        "env",
        ".env",
        "dist",
        "build",
        "target",
        ".tox",
        ".pytest_cache",
        ".mypy_cache",
        "vendor",
    }

    for root, dirs, files in os.walk(repo_path):
        # Skip excluded directories
        dirs[:] = [d for d in dirs if d not in skip_dirs and not d.startswith(".")]

        root_path = Path(root)
        rel_dir = root_path.relative_to(repo_path)
        dir_str = "." if rel_dir == Path(".") else str(rel_dir)

        for filename in files:
            if filename in INDICATOR_FILES:
                techs = INDICATOR_FILES[filename]
                marker_result = {
                    "directory": dir_str,
                    "marker_file": filename,
                    "languages": list(techs.get("languages", [])),
                    "package_managers": list(techs.get("package_managers", [])),
                    "frameworks": list(techs.get("frameworks", [])),
                    "infrastructure": list(techs.get("infrastructure", [])),
                }
                results.append(marker_result)

    return results


def detect_from_glob_patterns(repo_path: Path) -> dict[str, set[str]]:
    """Detect technologies from glob patterns."""
    from .package_parsers import match_frameworks, parse_dependencies

    results: dict[str, set[str]] = {
        "languages": set(),
        "package_managers": set(),
        "frameworks": set(),
        "infrastructure": set(),
    }

    for pattern, techs in GLOB_PATTERNS.items():
        matched_files = list(repo_path.glob(pattern)) + list(
            repo_path.glob(f"**/{pattern}")
        )
        if matched_files:
            for category, items in techs.items():
                results[category].update(items)

        # Parse matched files for framework dependencies
        for filepath in matched_files:
            try:
                content = filepath.read_text(encoding="utf-8", errors="ignore")
                deps = parse_dependencies(filepath.name, content)
                results["frameworks"].update(match_frameworks(deps, FRAMEWORK_PATTERNS))
            except (OSError, PermissionError):
                continue

    return results


def detect_kubernetes(repo_path: Path) -> bool:
    """Check if repository contains Kubernetes manifests."""
    yaml_files = list(repo_path.glob("**/*.yaml")) + list(repo_path.glob("**/*.yml"))

    for yaml_file in yaml_files:
        try:
            content = yaml_file.read_text(encoding="utf-8", errors="ignore")
            for marker in K8S_MARKERS:
                if marker in content:
                    return True
        except (OSError, PermissionError):
            continue

    return False


def detect_languages_from_extensions(repo_path: Path) -> set[str]:
    """Detect languages by scanning file extensions."""
    languages: set[str] = set()

    # Walk the repository, skipping common non-source directories
    skip_dirs = {
        ".git",
        "node_modules",
        "__pycache__",
        ".venv",
        "venv",
        "env",
        ".env",
        "dist",
        "build",
        "target",
        ".tox",
        ".pytest_cache",
        ".mypy_cache",
        "vendor",
    }

    for root, dirs, files in os.walk(repo_path):
        # Skip excluded directories
        dirs[:] = [d for d in dirs if d not in skip_dirs and not d.startswith(".")]

        for filename in files:
            ext = Path(filename).suffix.lower()
            if ext in EXTENSION_LANGUAGES:
                languages.add(EXTENSION_LANGUAGES[ext])

    return languages


def detect_frameworks_from_packages(repo_path: Path) -> set[str]:
    """Detect frameworks from package files."""
    from .package_parsers import match_frameworks, parse_dependencies

    frameworks: set[str] = set()

    config_files = [
        "pyproject.toml",
        "requirements.txt",
        "setup.py",
        "Pipfile",
        "package.json",
        "pom.xml",
        "build.gradle",
        "build.gradle.kts",
        "go.mod",
        "Cargo.toml",
    ]

    for filename in config_files:
        filepath = repo_path / filename
        if filepath.exists():
            try:
                content = filepath.read_text(encoding="utf-8", errors="ignore")
                deps = parse_dependencies(filename, content)
                frameworks.update(match_frameworks(deps, FRAMEWORK_PATTERNS))
            except (OSError, PermissionError):
                continue

    return frameworks


def detect_frameworks_for_file(filepath: Path) -> list[str]:
    """Detect frameworks from a specific package file."""
    from .package_parsers import match_frameworks, parse_dependencies

    if not filepath.exists():
        return []

    try:
        content = filepath.read_text(encoding="utf-8", errors="ignore")
    except (OSError, PermissionError):
        return []

    deps = parse_dependencies(filepath.name, content)
    return match_frameworks(deps, FRAMEWORK_PATTERNS)
