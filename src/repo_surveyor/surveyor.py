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
from .integration_concretiser import (
    ConcretisationResult,
    concretise_integration_signals,
)
from .integration_detector import IntegrationDetectorResult, detect_integrations
from .integration_patterns import Language
from .pipeline_timer import NullPipelineTimer, PipelineTimer
from .report import DirectoryMarker, SurveyReport
from .symbol_resolver import ResolutionResult, resolve_integration_signals
from .training.signal_classifier import NullSignalClassifier, SignalClassifier

logger = logging.getLogger(__name__)


def _normalise_languages(
    languages: list[str | Language],
) -> tuple[list[str], list[Language]]:
    """Split a mixed language list into CTags strings and Language enums.

    str values are kept as-is for CTags and looked up via Language.from_name()
    for integration detection (gracefully skipped if no matching enum member).
    Language values use .value for CTags and are kept as-is for integration detection.

    Args:
        languages: Mixed list of language name strings and Language enum members.

    Returns:
        Tuple of (ctags_languages, integration_languages).
    """
    ctags_languages: list[str] = []
    integration_languages: list[Language] = []
    for lang in languages:
        if isinstance(lang, Language):
            ctags_languages.append(lang.value)
            integration_languages.append(lang)
        else:
            ctags_languages.append(lang)
            resolved = Language.from_name(lang)
            if resolved is not None:
                integration_languages.append(resolved)
    return ctags_languages, integration_languages


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
    languages: list[str | Language] = [],
    extra_skip_dirs: list[str] = [],
    timer: PipelineTimer = NullPipelineTimer(),
    classifier: SignalClassifier = NullSignalClassifier(),
) -> tuple[
    SurveyReport,
    CTagsResult,
    IntegrationDetectorResult,
    ResolutionResult,
    ConcretisationResult,
]:
    """Run tech_stacks(), coarse_structure(), detect_integrations(), symbol resolution, and concretisation.

    Convenience function that orchestrates the full analysis pipeline
    without requiring a Neo4j connection.

    Args:
        repo_path: Path to the repository to analyze.
        languages: Languages to filter both coarse_structure() and
                   detect_integrations(). Accepts CTags language name strings
                   (e.g., "Java") and/or Language enum members. Defaults to all.
        extra_skip_dirs: Additional directory names to skip during scanning,
                         appended to the default skip list.
        timer: Pipeline timing observer for recording stage durations.
        classifier: Trained SignalClassifier for ML-based concretisation.
                    Defaults to NullSignalClassifier (labels all signals NOT_DEFINITE).

    Returns:
        Tuple of (SurveyReport, CTagsResult, IntegrationDetectorResult, ResolutionResult, ConcretisationResult).
    """
    ctags_languages, integration_languages = _normalise_languages(languages)
    surveyor = RepoSurveyor(repo_path)

    timer.stage_started("tech_stacks")
    tech_report = surveyor.tech_stacks(extra_skip_dirs=extra_skip_dirs)
    timer.stage_completed("tech_stacks")
    logger.info("Tech stacks completed")

    timer.stage_started("coarse_structure")
    structure_result = surveyor.coarse_structure(languages=ctags_languages)
    timer.stage_completed("coarse_structure")
    logger.info("Coarse structure completed")

    directory_frameworks = {
        m.directory: m.frameworks for m in tech_report.directory_markers if m.frameworks
    }
    timer.stage_started("integration_detection")
    integration_result = detect_integrations(
        repo_path,
        languages=integration_languages,
        directory_frameworks=directory_frameworks,
        extra_skip_dirs=extra_skip_dirs,
        timer=timer,
    )
    timer.stage_completed("integration_detection")
    logger.info(
        "Integration detection completed: %d points in %d files",
        len(integration_result.integration_points),
        integration_result.files_scanned,
    )

    timer.stage_started("symbol_resolution")
    resolution = resolve_integration_signals(
        structure_result, integration_result, repo_path
    )
    timer.stage_completed("symbol_resolution")
    logger.info(
        "Symbol resolution completed: %d resolved, %d unresolved, %d profiles",
        len(resolution.resolved),
        len(resolution.unresolved),
        len(resolution.profiles),
    )

    timer.stage_started("signal_concretisation")
    concretisation = concretise_integration_signals(integration_result, classifier)
    timer.stage_completed("signal_concretisation")
    logger.info(
        "Signal concretisation completed: %d submitted, %d definite, %d discarded",
        concretisation.signals_submitted,
        concretisation.signals_definite,
        concretisation.signals_discarded,
    )

    return tech_report, structure_result, integration_result, resolution, concretisation
