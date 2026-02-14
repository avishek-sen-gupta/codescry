"""Main RepoSurveyor class for analyzing repositories."""

import logging
from pathlib import Path

from .constants import MarkerKey, TechCategory
from .ctags import CTagsConfig, CTagsResult, run_ctags
from .detectors import (
    detect_frameworks_for_file,
    detect_from_glob_patterns,
    detect_indicator_files_with_directories,
    detect_kubernetes,
    detect_languages_from_extensions,
)
from .integration_detector import IntegrationDetectorResult, detect_integrations
from .report import DirectoryMarker, SurveyReport

logger = logging.getLogger(__name__)


class RepoSurveyor:
    """Analyze a repository and detect its technology stack."""

    def __init__(self, repo_path: str) -> None:
        """Initialize the surveyor with a repository path.

        Args:
            repo_path: Path to the repository to analyze.

        Raises:
            ValueError: If the path does not exist or is not a directory.
        """
        self.repo_path = Path(repo_path).resolve()

        if not self.repo_path.exists():
            raise ValueError(f"Path does not exist: {self.repo_path}")
        if not self.repo_path.is_dir():
            raise ValueError(f"Path is not a directory: {self.repo_path}")

    def tech_stacks(self, extra_skip_dirs: list[str] = []) -> SurveyReport:
        """Analyze the repository and return a survey report.

        Args:
            extra_skip_dirs: Additional directory names to skip during scanning,
                             appended to the default skip list.

        Returns:
            SurveyReport containing detected technologies.
        """
        # Initialize result sets
        languages: set[str] = set()
        package_managers: set[str] = set()
        frameworks: set[str] = set()
        infrastructure: set[str] = set()

        # Detect indicator files with directory associations
        dir_markers_data = detect_indicator_files_with_directories(
            self.repo_path, extra_skip_dirs=extra_skip_dirs
        )
        directory_markers: list[DirectoryMarker] = []

        for marker_data in dir_markers_data:
            # Aggregate into global sets
            languages.update(marker_data[TechCategory.LANGUAGES])
            package_managers.update(marker_data[TechCategory.PACKAGE_MANAGERS])
            frameworks.update(marker_data[TechCategory.FRAMEWORKS])
            infrastructure.update(marker_data[TechCategory.INFRASTRUCTURE])

            # Detect frameworks from the specific marker file
            marker_file_path = (
                self.repo_path
                / marker_data[MarkerKey.DIRECTORY]
                / marker_data[MarkerKey.MARKER_FILE]
            )
            detected_frameworks = detect_frameworks_for_file(marker_file_path)
            frameworks.update(detected_frameworks)

            # Add frameworks to the marker data
            marker_frameworks = list(
                set(marker_data[TechCategory.FRAMEWORKS] + detected_frameworks)
            )

            directory_markers.append(
                DirectoryMarker(
                    directory=marker_data[MarkerKey.DIRECTORY],
                    marker_file=marker_data[MarkerKey.MARKER_FILE],
                    languages=marker_data[TechCategory.LANGUAGES],
                    package_managers=marker_data[TechCategory.PACKAGE_MANAGERS],
                    frameworks=marker_frameworks,
                    infrastructure=marker_data[TechCategory.INFRASTRUCTURE],
                )
            )

        # Detect from glob patterns
        glob_results = detect_from_glob_patterns(self.repo_path)
        languages.update(glob_results[TechCategory.LANGUAGES])
        package_managers.update(glob_results[TechCategory.PACKAGE_MANAGERS])
        frameworks.update(glob_results[TechCategory.FRAMEWORKS])
        infrastructure.update(glob_results[TechCategory.INFRASTRUCTURE])

        # Detect Kubernetes
        if detect_kubernetes(self.repo_path):
            infrastructure.add("Kubernetes")

        # Detect languages from file extensions
        languages.update(detect_languages_from_extensions(self.repo_path))

        return SurveyReport(
            repo_path=str(self.repo_path),
            languages=sorted(languages),
            package_managers=sorted(package_managers),
            frameworks=sorted(frameworks),
            infrastructure=sorted(infrastructure),
            directory_markers=directory_markers,
        )

    def coarse_structure(
        self,
        languages: list[str] = [],
        exclude_patterns: list[str] = [
            ".git",
            ".idea",
            "target",
            "node_modules",
            "__pycache__",
            ".venv",
            "venv",
        ],
        extra_fields: str = "+n+k+S+z+K+l",
        extras: str = "+q",
        verbose: bool = False,
    ) -> CTagsResult:
        """Run CTags on the repository to extract code symbols.

        Args:
            languages: List of languages to analyze (e.g., ["Java", "Python"]).
                       If None, CTags will analyze all supported languages.
            exclude_patterns: Patterns to exclude from analysis.
                              Defaults to common build/dependency directories.
            extra_fields: CTags field flags. Default is "+n+k+S+z+K+l" which includes:
                          n=line number, k=kind, S=signature, z=kind (long),
                          K=kind (full), l=language.
            extras: CTags extras flags. Default is "+q" for qualified tags.
            verbose: Whether to run CTags in verbose mode.

        Returns:
            CTagsResult containing parsed symbol entries and execution metadata.

        Raises:
            FileNotFoundError: If ctags is not installed or not in PATH.
        """
        config = CTagsConfig(
            languages=languages,
            exclude_patterns=exclude_patterns,
            extra_fields=extra_fields,
            extras=extras,
            verbose=verbose,
        )
        return run_ctags(self.repo_path, config)


def survey(
    repo_path: str,
    languages: list[str] = [],
) -> tuple[SurveyReport, CTagsResult, IntegrationDetectorResult]:
    """Run tech_stacks(), coarse_structure(), and detect_integrations().

    Convenience function that orchestrates the full analysis pipeline
    without requiring a Neo4j connection.

    Args:
        repo_path: Path to the repository to analyze.
        languages: Languages to pass to coarse_structure()
                   (e.g., ["Java", "Python"]). Defaults to all.

    Returns:
        Tuple of (SurveyReport, CTagsResult, IntegrationDetectorResult).
    """
    surveyor = RepoSurveyor(repo_path)

    tech_report = surveyor.tech_stacks()
    logger.info("Tech stacks completed")

    structure_result = surveyor.coarse_structure(languages=languages)
    logger.info("Coarse structure completed")

    directory_frameworks = {
        m.directory: m.frameworks
        for m in tech_report.directory_markers
        if m.frameworks
    }
    integration_result = detect_integrations(
        repo_path, directory_frameworks=directory_frameworks
    )
    logger.info(
        "Integration detection completed: %d points in %d files",
        len(integration_result.integration_points),
        integration_result.files_scanned,
    )

    return tech_report, structure_result, integration_result
