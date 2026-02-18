"""Integration point detection from file contents.

Detects system integration points (HTTP, SOAP, messaging, sockets, database)
from source file contents using regex pattern matching.
"""

import json
import re
from dataclasses import dataclass
from enum import Enum
from itertools import chain
from pathlib import Path
from typing import Iterator

from .integration_patterns import (
    Confidence,
    IntegrationType,
    Language,
    EXTENSION_TO_LANGUAGE,
    get_patterns_for_language,
    get_import_patterns_for_framework,
    get_directory_patterns,
)
from .pipeline_timer import NullPipelineTimer, PipelineTimer
from .syntax_zone import SyntaxRangeMap, SyntaxZone, classify_line, parse_file_zones

_SKIP_ZONES = frozenset(
    {
        SyntaxZone.COMMENT,
        SyntaxZone.STRING_LITERAL,
        SyntaxZone.IMPORT,
        SyntaxZone.PACKAGE_DECLARATION,
    }
)

DEFAULT_SKIP_DIRS = frozenset(
    {
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
    source: str


@dataclass(frozen=True)
class IntegrationDetectorResult:
    """Result of integration detection."""

    integration_points: list[IntegrationSignal]
    files_scanned: int

    def to_json(self, indent: int | None = 2) -> str:
        """Generate a JSON representation of the result."""
        return json.dumps(
            {
                "files_scanned": self.files_scanned,
                "integration_points": [
                    {
                        "integration_type": point.integration_type.value,
                        "confidence": point.confidence.value,
                        "matched_pattern": point.matched_pattern,
                        "entity_type": point.entity_type.value,
                        "source": point.source,
                        "match": {
                            "file_path": point.match.file_path,
                            "line_number": point.match.line_number,
                            "line_content": point.match.line_content,
                            "language": (
                                point.match.language.value
                                if isinstance(point.match.language, Language)
                                else point.match.language
                            ),
                        },
                    }
                    for point in self.integration_points
                ],
            },
            indent=indent,
        )


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
    return list(
        chain.from_iterable(
            directory_frameworks.get("." if parent == Path(".") else str(parent), [])
            for parent in [rel_path.parent, *rel_path.parent.parents]
        )
    )


def _file_has_framework_import(content: str, import_patterns: tuple[str, ...]) -> bool:
    """Check whether file content contains at least one framework import.

    Args:
        content: The full file content.
        import_patterns: Regex patterns to search for.

    Returns:
        True if any pattern matches any line in the content.
    """
    return any(
        re.search(pattern, line)
        for line in content.splitlines()
        for pattern in import_patterns
    )


def _filter_frameworks_by_imports(
    content: str, language: Language | None, frameworks: list[str]
) -> list[str]:
    """Filter frameworks to only those whose imports are present in the file.

    Frameworks with empty ``import_patterns`` (ungated) are always kept.

    Args:
        content: The full file content.
        language: The programming language of the file.
        frameworks: List of candidate framework names.

    Returns:
        Filtered list of framework names.
    """
    return [
        fw
        for fw in frameworks
        if (import_patterns := get_import_patterns_for_framework(language, fw)) == ()
        or _file_has_framework_import(content, import_patterns)
    ]


def scan_file_for_integrations(
    file_path: Path,
    frameworks: list[str] = [],
) -> Iterator[IntegrationSignal]:
    """Scan a single file for integration points.

    Args:
        file_path: Path to the file to scan.
        frameworks: List of active framework names for this file's context.
                    These should already be import-gated by the caller;
                    this function applies them directly without further filtering.

    Yields:
        IntegrationSignal instances for each match found.
    """
    try:
        content = file_path.read_text(encoding="utf-8", errors="ignore")
    except (OSError, IOError):
        return

    language = get_language_from_extension(str(file_path))
    patterns = get_patterns_for_language(language, frameworks)

    range_map = (
        parse_file_zones(content.encode("utf-8"), language)
        if language is not None
        else SyntaxRangeMap(ranges=())
    )

    lines = content.splitlines()

    for line_num, line in enumerate(lines, start=1):
        if classify_line(range_map, line_num) in _SKIP_ZONES:
            continue
        for integration_type, type_patterns in patterns.items():
            for pattern, confidence, source in type_patterns:
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
                        source=source,
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
    languages: list[Language] = [],
    extra_skip_dirs: list[str] = [],
) -> Iterator[Path]:
    """Get source files from a repository.

    Args:
        repo_path: Path to the repository.
        languages: List of Language enums to filter by. Empty means all.
        extra_skip_dirs: Additional directory names to skip, appended to
                         DEFAULT_SKIP_DIRS.

    Yields:
        Paths to source files.
    """
    skip_dirs = DEFAULT_SKIP_DIRS | set(extra_skip_dirs)

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


def _build_import_gated_framework_map(
    source_files: Iterator[Path],
    repo_path: Path,
    directory_frameworks: dict[str, list[str]],
) -> dict[Path, list[str]]:
    """Build a mapping from each source file to its import-gated frameworks.

    For each file, determines the candidate frameworks from its directory
    hierarchy, reads the file content, and filters frameworks by checking
    for matching import statements.

    Args:
        source_files: Iterator of source file paths.
        repo_path: Absolute path to the repository root.
        directory_frameworks: Mapping of directory paths (relative to repo root)
                              to their detected frameworks.

    Returns:
        Dict mapping each file path to its import-gated framework list.
    """
    return {
        file_path: _filter_frameworks_by_imports(
            content,
            get_language_from_extension(str(file_path)),
            _find_frameworks_for_file(file_path, repo_path, directory_frameworks),
        )
        for file_path in source_files
        if (content := _read_file_content(file_path)) is not None
    }


def _read_file_content(file_path: Path) -> str | None:
    """Read file content, returning None on I/O errors.

    Args:
        file_path: Path to the file to read.

    Returns:
        File content as a string, or None if the file could not be read.
    """
    try:
        return file_path.read_text(encoding="utf-8", errors="ignore")
    except (OSError, IOError):
        return None


def detect_integrations(
    repo_path: str | Path,
    languages: list[Language] = [],
    directory_frameworks: dict[str, list[str]] = {},
    extra_skip_dirs: list[str] = [],
    timer: PipelineTimer = NullPipelineTimer(),
) -> IntegrationDetectorResult:
    """Detect integration points from repository file contents.

    Scans source files for patterns indicating system integrations.
    Framework-specific patterns are applied based on the frameworks
    active in each file's directory.

    Args:
        repo_path: Path to the repository to scan.
        languages: List of Language enums to scan (e.g., [Language.RUST, Language.PYTHON]).
                   Empty list scans all supported languages.
        directory_frameworks: Mapping of directory paths (relative to repo root,
                              e.g., "." or "backend") to their detected framework names
                              (e.g., ["FastAPI", "Django"]). Files in those
                              directories will be scanned with framework-specific patterns.
        extra_skip_dirs: Additional directory names to skip, appended to
                         DEFAULT_SKIP_DIRS.
        timer: Pipeline timing observer for recording sub-stage durations.

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

    # Build import-gated framework map
    timer.stage_started("integration_detection.import_gating")
    file_frameworks = _build_import_gated_framework_map(
        _get_source_files(repo_path, languages, extra_skip_dirs),
        repo_path,
        directory_frameworks,
    )
    timer.stage_completed("integration_detection.import_gating")

    # Scan source files
    files_scanned = 0
    timer.stage_started("integration_detection.file_scanning")
    for file_path in _get_source_files(repo_path, languages, extra_skip_dirs):
        files_scanned += 1
        frameworks = file_frameworks.get(file_path, [])
        integration_points.extend(scan_file_for_integrations(file_path, frameworks))
    timer.stage_completed("integration_detection.file_scanning")

    # Scan directory names
    timer.stage_started("integration_detection.directory_classification")
    scanned_dirs: set[str] = set()
    for file_path in _get_source_files(repo_path, languages, extra_skip_dirs):
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
                            source="common",
                        )
                    )
    timer.stage_completed("integration_detection.directory_classification")

    return IntegrationDetectorResult(
        integration_points=integration_points,
        files_scanned=files_scanned,
    )
