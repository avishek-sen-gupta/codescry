"""Main RepoSurveyor class for analyzing repositories."""

from pathlib import Path

from .detectors import (
    detect_frameworks_for_file,
    detect_from_glob_patterns,
    detect_indicator_files_with_directories,
    detect_kubernetes,
    detect_languages_from_extensions,
)
from .report import DirectoryMarker, SurveyReport


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

    def survey(self) -> SurveyReport:
        """Analyze the repository and return a survey report.

        Returns:
            SurveyReport containing detected technologies.
        """
        # Initialize result sets
        languages: set[str] = set()
        package_managers: set[str] = set()
        frameworks: set[str] = set()
        infrastructure: set[str] = set()

        # Detect indicator files with directory associations
        dir_markers_data = detect_indicator_files_with_directories(self.repo_path)
        directory_markers: list[DirectoryMarker] = []

        for marker_data in dir_markers_data:
            # Aggregate into global sets
            languages.update(marker_data["languages"])
            package_managers.update(marker_data["package_managers"])
            frameworks.update(marker_data["frameworks"])
            infrastructure.update(marker_data["infrastructure"])

            # Detect frameworks from the specific marker file
            marker_file_path = self.repo_path / marker_data["directory"] / marker_data["marker_file"]
            detected_frameworks = detect_frameworks_for_file(marker_file_path)
            frameworks.update(detected_frameworks)

            # Add frameworks to the marker data
            marker_frameworks = list(set(marker_data["frameworks"] + detected_frameworks))

            directory_markers.append(
                DirectoryMarker(
                    directory=marker_data["directory"],
                    marker_file=marker_data["marker_file"],
                    languages=marker_data["languages"],
                    package_managers=marker_data["package_managers"],
                    frameworks=marker_frameworks,
                    infrastructure=marker_data["infrastructure"],
                )
            )

        # Detect from glob patterns
        glob_results = detect_from_glob_patterns(self.repo_path)
        languages.update(glob_results["languages"])
        package_managers.update(glob_results["package_managers"])
        frameworks.update(glob_results["frameworks"])
        infrastructure.update(glob_results["infrastructure"])

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
