"""Integration point detection from file contents.

Detects system integration points (HTTP, SOAP, messaging, sockets, database)
from source file contents using regex pattern matching.
"""

import re
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Iterator

from .integration_patterns import (
    Confidence,
    IntegrationType,
    Language,
    EXTENSION_TO_LANGUAGE,
    get_patterns_for_language,
    get_directory_patterns,
)


class EntityType(Enum):
    """Type of entity where integration was detected."""

    FILE_CONTENT = "file_content"
    DIRECTORY = "directory"


@dataclass(frozen=True)
class FileMatch:
    """A match found in a file."""

    file_path: str
    line_number: int
    line_content: str
    language: Language | None


@dataclass(frozen=True)
class IntegrationSignal:
    """A detected integration point."""

    match: FileMatch
    integration_type: IntegrationType
    confidence: Confidence
    matched_pattern: str
    entity_type: EntityType


@dataclass(frozen=True)
class IntegrationDetectorResult:
    """Result of integration detection."""

    integration_points: list[IntegrationSignal]
    files_scanned: int


def get_language_from_extension(file_path: str) -> Language | None:
    """Determine programming language from file extension.

    Args:
        file_path: Path to the file.

    Returns:
        Language enum value or None if unknown.
    """
    ext = Path(file_path).suffix.lower()
    return EXTENSION_TO_LANGUAGE.get(ext)


def _find_frameworks_for_file(
    file_path: Path,
    repo_path: Path,
    directory_frameworks: dict[str, list[str]],
) -> list[str]:
    """Find applicable frameworks for a file based on its directory hierarchy.

    Walks up from the file's parent directory to the repo root, collecting
    frameworks from all matching directories in the mapping.

    Args:
        file_path: Absolute path to the file.
        repo_path: Absolute path to the repository root.
        directory_frameworks: Mapping of directory paths (relative to repo root)
                              to their detected frameworks.

    Returns:
        List of framework names applicable to this file.
    """
    rel_path = file_path.relative_to(repo_path)
    frameworks: list[str] = []
    for parent in [rel_path.parent, *rel_path.parent.parents]:
        dir_key = "." if parent == Path(".") else str(parent)
        if dir_key in directory_frameworks:
            frameworks.extend(directory_frameworks[dir_key])
    return frameworks


def scan_file_for_integrations(
    file_path: Path,
    frameworks: list[str] = [],
) -> Iterator[IntegrationSignal]:
    """Scan a single file for integration points.

    Args:
        file_path: Path to the file to scan.
        frameworks: List of active framework names for this file's context.
                    Framework-specific patterns are included for these frameworks.

    Yields:
        IntegrationSignal instances for each match found.
    """
    try:
        content = file_path.read_text(encoding="utf-8", errors="ignore")
    except (OSError, IOError):
        return

    language = get_language_from_extension(str(file_path))
    patterns = get_patterns_for_language(language, frameworks)

    lines = content.splitlines()

    for line_num, line in enumerate(lines, start=1):
        for integration_type, type_patterns in patterns.items():
            for pattern, confidence in type_patterns:
                if re.search(pattern, line):
                    match = FileMatch(
                        file_path=str(file_path),
                        line_number=line_num,
                        line_content=line.strip(),
                        language=language,
                    )
                    yield IntegrationSignal(
                        match=match,
                        integration_type=integration_type,
                        confidence=confidence,
                        matched_pattern=pattern,
                        entity_type=EntityType.FILE_CONTENT,
                    )


def classify_directory(
    directory_name: str,
) -> list[tuple[IntegrationType, Confidence, str]]:
    """Classify a directory name into integration types.

    Args:
        directory_name: The directory name to classify.

    Returns:
        List of (integration_type, confidence, matched_pattern) tuples.
    """
    matches: list[tuple[IntegrationType, Confidence, str]] = []
    directory_patterns = get_directory_patterns()

    for integration_type, patterns in directory_patterns.items():
        for pattern in patterns:
            if re.search(pattern, directory_name):
                matches.append(
                    (integration_type, Confidence.MEDIUM, f"directory:{pattern}")
                )
                break

    return matches


def _get_source_files(
    repo_path: Path,
    languages: list[Language] | None = None,
) -> Iterator[Path]:
    """Get source files from a repository.

    Args:
        repo_path: Path to the repository.
        languages: Optional list of Language enums to filter by.

    Yields:
        Paths to source files.
    """
    # Directories to skip
    skip_dirs = {
        ".git",
        ".idea",
        ".vscode",
        "node_modules",
        "__pycache__",
        ".venv",
        "venv",
        "target",
        "build",
        "dist",
        ".tox",
        ".pytest_cache",
        ".mypy_cache",
    }

    # Get allowed extensions based on languages
    if languages:
        language_set = set(languages)
        allowed_extensions = {
            ext for ext, lang in EXTENSION_TO_LANGUAGE.items() if lang in language_set
        }
    else:
        allowed_extensions = set(EXTENSION_TO_LANGUAGE.keys())

    for path in repo_path.rglob("*"):
        if path.is_file():
            # Skip files in excluded directories
            if any(skip_dir in path.parts for skip_dir in skip_dirs):
                continue

            # Check extension
            if path.suffix.lower() in allowed_extensions:
                yield path


def detect_integrations(
    repo_path: str | Path,
    languages: list[Language] | None = None,
    directory_frameworks: dict[str, list[str]] = {},
) -> IntegrationDetectorResult:
    """Detect integration points from repository file contents.

    Scans source files for patterns indicating system integrations.
    Framework-specific patterns are applied based on the frameworks
    active in each file's directory.

    Args:
        repo_path: Path to the repository to scan.
        languages: Optional list of Language enums to scan (e.g., [Language.RUST, Language.PYTHON]).
                   If None, scans all supported languages.
        directory_frameworks: Mapping of directory paths (relative to repo root,
                              e.g., "." or "backend") to their detected framework names
                              (e.g., ["FastAPI", "Django"]). Files in those
                              directories will be scanned with framework-specific patterns.

    Returns:
        IntegrationDetectorResult with detected integration points.
    """
    repo_path = Path(repo_path)

    if not repo_path.is_dir():
        return IntegrationDetectorResult(
            integration_points=[],
            files_scanned=0,
        )

    integration_points: list[IntegrationSignal] = []
    files_scanned = 0

    # Scan source files
    for file_path in _get_source_files(repo_path, languages):
        files_scanned += 1
        frameworks = _find_frameworks_for_file(
            file_path, repo_path, directory_frameworks
        )
        integration_points.extend(scan_file_for_integrations(file_path, frameworks))

    # Scan directory names
    scanned_dirs: set[str] = set()
    for file_path in _get_source_files(repo_path, languages):
        for parent in file_path.relative_to(repo_path).parents:
            dir_name = parent.name
            if dir_name and dir_name not in scanned_dirs:
                scanned_dirs.add(dir_name)
                for int_type, confidence, pattern in classify_directory(dir_name):
                    # Create a placeholder match for the directory
                    match = FileMatch(
                        file_path=str(repo_path / parent),
                        line_number=0,
                        line_content="",
                        language="",
                    )
                    integration_points.append(
                        IntegrationSignal(
                            match=match,
                            integration_type=int_type,
                            confidence=confidence,
                            matched_pattern=pattern,
                            entity_type=EntityType.DIRECTORY,
                        )
                    )

    return IntegrationDetectorResult(
        integration_points=integration_points,
        files_scanned=files_scanned,
    )
