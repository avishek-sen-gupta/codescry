"""Unit tests for RepoSurveyor and survey() using synthetic fixture repos."""

from pathlib import Path

import pytest

from repo_surveyor.pipeline_timer import PipelineTimingObserver
from repo_surveyor.surveyor import RepoSurveyor, survey


def _create_python_repo(tmp_path: Path) -> Path:
    """Create a minimal Python repo with indicator files and source."""
    (tmp_path / "pyproject.toml").write_text(
        '[project]\ndependencies = ["fastapi"]\n'
    )
    src = tmp_path / "src"
    src.mkdir()
    (src / "app.py").write_text(
        "from fastapi import FastAPI\n"
        "\n"
        "app = FastAPI()\n"
        "\n"
        "@app.get('/health')\n"
        "def health():\n"
        "    return {'status': 'ok'}\n"
    )
    return tmp_path


def _create_monorepo(tmp_path: Path) -> Path:
    """Create a monorepo with Python backend and JS frontend."""
    backend = tmp_path / "backend"
    backend.mkdir()
    (backend / "pyproject.toml").write_text(
        '[project]\ndependencies = ["django"]\n'
    )
    (backend / "views.py").write_text(
        "from django.http import HttpResponse\n"
        "\n"
        "def index(request):\n"
        "    return HttpResponse('hello')\n"
    )

    frontend = tmp_path / "frontend"
    frontend.mkdir()
    (frontend / "package.json").write_text(
        '{"dependencies": {"react": "^18.0.0"}}\n'
    )
    (frontend / "index.js").write_text(
        "import React from 'react';\n"
    )
    return tmp_path


class TestRepoSurveyorInit:
    """Tests for RepoSurveyor initialisation and validation."""

    def test_raises_for_nonexistent_path(self, tmp_path: Path) -> None:
        """Should raise ValueError for a path that doesn't exist."""
        with pytest.raises(ValueError, match="does not exist"):
            RepoSurveyor(str(tmp_path / "nonexistent"))

    def test_raises_for_file_path(self, tmp_path: Path) -> None:
        """Should raise ValueError for a file, not a directory."""
        f = tmp_path / "somefile.txt"
        f.write_text("hello")
        with pytest.raises(ValueError, match="not a directory"):
            RepoSurveyor(str(f))

    def test_resolves_path(self, tmp_path: Path) -> None:
        """Should resolve the repo path to absolute."""
        surveyor = RepoSurveyor(str(tmp_path))
        assert Path(surveyor.repo_path).is_absolute()


class TestTechStacks:
    """Tests for RepoSurveyor.tech_stacks() with synthetic repos."""

    def test_detects_python_and_poetry(self, tmp_path: Path) -> None:
        """Should detect Python and Poetry from pyproject.toml."""
        repo = _create_python_repo(tmp_path)
        report = RepoSurveyor(str(repo)).tech_stacks()

        assert "Python" in report.languages
        assert "Poetry" in report.package_managers

    def test_detects_frameworks_from_config(self, tmp_path: Path) -> None:
        """Should detect FastAPI from pyproject.toml dependencies."""
        repo = _create_python_repo(tmp_path)
        report = RepoSurveyor(str(repo)).tech_stacks()

        assert "FastAPI" in report.frameworks

    def test_detects_languages_from_extensions(self, tmp_path: Path) -> None:
        """Should detect Python from .py file extensions."""
        repo = _create_python_repo(tmp_path)
        report = RepoSurveyor(str(repo)).tech_stacks()

        assert "Python" in report.languages

    def test_directory_markers_created(self, tmp_path: Path) -> None:
        """Should create directory markers for each indicator file."""
        repo = _create_python_repo(tmp_path)
        report = RepoSurveyor(str(repo)).tech_stacks()

        assert len(report.directory_markers) >= 1
        pyproject_markers = [
            m for m in report.directory_markers if m.marker_file == "pyproject.toml"
        ]
        assert len(pyproject_markers) == 1
        assert pyproject_markers[0].directory == "."

    def test_monorepo_multiple_markers(self, tmp_path: Path) -> None:
        """Should detect markers in multiple subdirectories."""
        repo = _create_monorepo(tmp_path)
        report = RepoSurveyor(str(repo)).tech_stacks()

        dirs = {m.directory for m in report.directory_markers}
        assert "backend" in dirs
        assert "frontend" in dirs

    def test_monorepo_detects_both_languages(self, tmp_path: Path) -> None:
        """Should detect languages from both subdirectories."""
        repo = _create_monorepo(tmp_path)
        report = RepoSurveyor(str(repo)).tech_stacks()

        assert "Python" in report.languages
        assert "JavaScript" in report.languages

    def test_extra_skip_dirs(self, tmp_path: Path) -> None:
        """Should skip extra directories when specified."""
        repo = _create_monorepo(tmp_path)
        report = RepoSurveyor(str(repo)).tech_stacks(extra_skip_dirs=["frontend"])

        dirs = {m.directory for m in report.directory_markers}
        assert "frontend" not in dirs

    def test_kubernetes_detection(self, tmp_path: Path) -> None:
        """Should detect Kubernetes from yaml manifests."""
        (tmp_path / "deploy.yaml").write_text(
            "apiVersion: apps/v1\nkind: Deployment\n"
        )
        report = RepoSurveyor(str(tmp_path)).tech_stacks()

        assert "Kubernetes" in report.infrastructure

    def test_no_kubernetes_without_manifests(self, tmp_path: Path) -> None:
        """Should not detect Kubernetes without k8s manifests."""
        (tmp_path / "config.yaml").write_text("name: my-app\n")
        report = RepoSurveyor(str(tmp_path)).tech_stacks()

        assert "Kubernetes" not in report.infrastructure

    def test_empty_repo(self, tmp_path: Path) -> None:
        """Should return empty report for an empty directory."""
        report = RepoSurveyor(str(tmp_path)).tech_stacks()

        assert report.languages == []
        assert report.package_managers == []
        assert report.frameworks == []
        assert report.infrastructure == []
        assert report.directory_markers == []

    def test_glob_pattern_detection(self, tmp_path: Path) -> None:
        """Should detect C# from .csproj via glob patterns."""
        (tmp_path / "MyApp.csproj").write_text("<Project></Project>")
        (tmp_path / "Program.cs").write_text("class Program {}\n")
        report = RepoSurveyor(str(tmp_path)).tech_stacks()

        assert "C#" in report.languages


class TestCoarseStructure:
    """Tests for RepoSurveyor.coarse_structure() with synthetic repos."""

    def test_extracts_python_symbols(self, tmp_path: Path) -> None:
        """Should extract Python function symbols via CTags."""
        repo = _create_python_repo(tmp_path)
        result = RepoSurveyor(str(repo)).coarse_structure(languages=["Python"])

        assert result.success
        function_names = {e.name for e in result.entries if e.kind == "function"}
        assert "health" in function_names

    def test_empty_repo_succeeds(self, tmp_path: Path) -> None:
        """Should succeed with no entries for an empty directory."""
        result = RepoSurveyor(str(tmp_path)).coarse_structure()

        assert result.success
        assert result.entries == []

    def test_language_filter(self, tmp_path: Path) -> None:
        """Should respect language filter."""
        repo = _create_monorepo(tmp_path)
        result = RepoSurveyor(str(repo)).coarse_structure(languages=["Python"])

        assert result.success
        languages = {e.language for e in result.entries if e.language}
        assert "JavaScript" not in languages


class TestSurveyFunction:
    """Tests for the survey() convenience function with synthetic repos."""

    def test_returns_three_results(self, tmp_path: Path) -> None:
        """survey() should return a SurveyReport, CTagsResult, and IntegrationDetectorResult."""
        repo = _create_python_repo(tmp_path)
        tech_report, structure_result, integration_result = survey(str(repo))

        assert "Python" in tech_report.languages
        assert structure_result.success
        assert integration_result.files_scanned > 0

    def test_wires_frameworks_to_integration_detection(self, tmp_path: Path) -> None:
        """survey() should pass detected frameworks to integration detection."""
        repo = _create_python_repo(tmp_path)
        tech_report, _, integration_result = survey(str(repo))

        assert "FastAPI" in tech_report.frameworks
        fastapi_points = [
            p for p in integration_result.integration_points
            if p.source == "FastAPI"
        ]
        assert len(fastapi_points) > 0

    def test_extra_skip_dirs_propagated(self, tmp_path: Path) -> None:
        """survey() should propagate extra_skip_dirs to both stacks and integration detection."""
        repo = _create_monorepo(tmp_path)
        tech_report, _, integration_result = survey(
            str(repo), extra_skip_dirs=["frontend"]
        )

        # Frontend should be skipped from tech stacks
        dirs = {m.directory for m in tech_report.directory_markers}
        assert "frontend" not in dirs

    def test_timer_records_stages(self, tmp_path: Path) -> None:
        """survey() should populate the timer with expected stage names."""
        repo = _create_python_repo(tmp_path)
        timer = PipelineTimingObserver()
        survey(str(repo), timer=timer)

        stage_names = [r.stage for r in timer.completed]
        assert "tech_stacks" in stage_names
        assert "coarse_structure" in stage_names
        assert "integration_detection" in stage_names
        assert "integration_detection.file_scanning" in stage_names
        assert "integration_detection.directory_classification" in stage_names

    def test_languages_filter_passed_to_ctags(self, tmp_path: Path) -> None:
        """survey() should pass languages to coarse_structure()."""
        repo = _create_monorepo(tmp_path)
        _, structure_result, _ = survey(str(repo), languages=["Python"])

        assert structure_result.success
        languages = {e.language for e in structure_result.entries if e.language}
        assert "JavaScript" not in languages
