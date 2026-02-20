"""Detection logic for various technologies."""

import os
from pathlib import Path

from repo_surveyor.core.constants import MarkerKey, TechCategory
from repo_surveyor.detection.language_plugin import PluginRegistry

_registry = PluginRegistry()

# Backward-compatible module-level exports (same variable names, same shapes)
INDICATOR_FILES = _registry.indicator_files()
EXTENSION_LANGUAGES = _registry.extension_languages()
FRAMEWORK_PATTERNS = _registry.all_framework_patterns()
GLOB_PATTERNS = _registry.glob_patterns()
K8S_MARKERS = _registry.k8s_markers()

DEFAULT_SKIP_DIRS = frozenset(
    {
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
)


def detect_from_indicator_files(repo_path: Path) -> dict[str, set[str]]:
    """Detect technologies from indicator files in root only."""
    results: dict[str, set[str]] = {
        TechCategory.LANGUAGES: set(),
        TechCategory.PACKAGE_MANAGERS: set(),
        TechCategory.FRAMEWORKS: set(),
        TechCategory.INFRASTRUCTURE: set(),
    }

    for filename, techs in INDICATOR_FILES.items():
        if (repo_path / filename).exists():
            for category, items in techs.items():
                results[category].update(items)

    return results


def detect_indicator_files_with_directories(
    repo_path: Path,
    extra_skip_dirs: list[str] = [],
) -> list[dict]:
    """Detect indicator files and associate them with their directories.

    Args:
        repo_path: Path to the repository root.
        extra_skip_dirs: Additional directory names to skip during scanning,
                         appended to DEFAULT_SKIP_DIRS.

    Returns a list of dicts with directory, marker_file, and detected technologies.
    """
    results: list[dict] = []

    skip_dirs = DEFAULT_SKIP_DIRS | set(extra_skip_dirs)

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
                    MarkerKey.DIRECTORY: dir_str,
                    MarkerKey.MARKER_FILE: filename,
                    TechCategory.LANGUAGES: list(techs.get(TechCategory.LANGUAGES, [])),
                    TechCategory.PACKAGE_MANAGERS: list(
                        techs.get(TechCategory.PACKAGE_MANAGERS, [])
                    ),
                    TechCategory.FRAMEWORKS: list(
                        techs.get(TechCategory.FRAMEWORKS, [])
                    ),
                    TechCategory.INFRASTRUCTURE: list(
                        techs.get(TechCategory.INFRASTRUCTURE, [])
                    ),
                }
                results.append(marker_result)

    return results


def detect_from_glob_patterns(repo_path: Path) -> dict[str, set[str]]:
    """Detect technologies from glob patterns."""
    from repo_surveyor.package_parsers import match_frameworks, parse_dependencies

    results: dict[str, set[str]] = {
        TechCategory.LANGUAGES: set(),
        TechCategory.PACKAGE_MANAGERS: set(),
        TechCategory.FRAMEWORKS: set(),
        TechCategory.INFRASTRUCTURE: set(),
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
                results[TechCategory.FRAMEWORKS].update(
                    match_frameworks(deps, FRAMEWORK_PATTERNS)
                )
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
    from repo_surveyor.package_parsers import match_frameworks, parse_dependencies

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
        "vcpkg.json",
        "conanfile.txt",
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
    from repo_surveyor.package_parsers import match_frameworks, parse_dependencies

    if not filepath.exists():
        return []

    try:
        content = filepath.read_text(encoding="utf-8", errors="ignore")
    except (OSError, PermissionError):
        return []

    deps = parse_dependencies(filepath.name, content)
    return match_frameworks(deps, FRAMEWORK_PATTERNS)
