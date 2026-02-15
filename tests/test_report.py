"""Tests for SurveyReport text and JSON output."""

import json

from repo_surveyor.report import DirectoryMarker, SurveyReport


class TestSurveyReportToText:
    """Tests for SurveyReport.to_text()."""

    def test_includes_repo_path(self) -> None:
        """to_text() should include the repository path header."""
        report = SurveyReport(repo_path="/test/repo")
        text = report.to_text()

        assert "Repository Survey: /test/repo" in text

    def test_lists_languages(self) -> None:
        """to_text() should list languages sorted alphabetically."""
        report = SurveyReport(
            repo_path="/test",
            languages=["Python", "Java", "Go"],
        )
        text = report.to_text()

        assert "Languages:" in text
        assert "  - Go" in text
        assert "  - Java" in text
        assert "  - Python" in text

    def test_lists_package_managers(self) -> None:
        """to_text() should list package managers."""
        report = SurveyReport(
            repo_path="/test",
            package_managers=["Poetry", "npm"],
        )
        text = report.to_text()

        assert "Package Managers:" in text
        assert "  - Poetry" in text
        assert "  - npm" in text

    def test_lists_frameworks(self) -> None:
        """to_text() should list frameworks."""
        report = SurveyReport(
            repo_path="/test",
            frameworks=["FastAPI", "React"],
        )
        text = report.to_text()

        assert "Frameworks:" in text
        assert "  - FastAPI" in text
        assert "  - React" in text

    def test_lists_infrastructure(self) -> None:
        """to_text() should list infrastructure."""
        report = SurveyReport(
            repo_path="/test",
            infrastructure=["Docker", "Kubernetes"],
        )
        text = report.to_text()

        assert "Infrastructure:" in text
        assert "  - Docker" in text
        assert "  - Kubernetes" in text

    def test_omits_empty_sections(self) -> None:
        """to_text() should not include headers for empty sections."""
        report = SurveyReport(
            repo_path="/test",
            languages=["Python"],
        )
        text = report.to_text()

        assert "Languages:" in text
        assert "Package Managers:" not in text
        assert "Frameworks:" not in text
        assert "Infrastructure:" not in text

    def test_directory_markers(self) -> None:
        """to_text() should render directory markers with all fields."""
        report = SurveyReport(
            repo_path="/test",
            directory_markers=[
                DirectoryMarker(
                    directory="backend",
                    marker_file="pyproject.toml",
                    languages=["Python"],
                    package_managers=["Poetry"],
                    frameworks=["FastAPI"],
                    infrastructure=["Docker"],
                ),
            ],
        )
        text = report.to_text()

        assert "Directory Markers:" in text
        assert "  backend/" in text
        assert "    Marker: pyproject.toml" in text
        assert "    Languages: Python" in text
        assert "    Package Managers: Poetry" in text
        assert "    Frameworks: FastAPI" in text
        assert "    Infrastructure: Docker" in text

    def test_directory_markers_sorted(self) -> None:
        """to_text() should sort directory markers by directory name."""
        report = SurveyReport(
            repo_path="/test",
            directory_markers=[
                DirectoryMarker(directory="frontend", marker_file="package.json"),
                DirectoryMarker(directory="backend", marker_file="pyproject.toml"),
            ],
        )
        text = report.to_text()
        backend_pos = text.index("backend/")
        frontend_pos = text.index("frontend/")
        assert backend_pos < frontend_pos

    def test_directory_marker_omits_empty_fields(self) -> None:
        """to_text() should not render empty marker fields."""
        report = SurveyReport(
            repo_path="/test",
            directory_markers=[
                DirectoryMarker(
                    directory=".",
                    marker_file="docker-compose.yml",
                    infrastructure=["Docker Compose"],
                ),
            ],
        )
        text = report.to_text()

        assert "    Marker: docker-compose.yml" in text
        assert "    Infrastructure: Docker Compose" in text
        assert "    Languages:" not in text
        assert "    Package Managers:" not in text
        assert "    Frameworks:" not in text

    def test_empty_report(self) -> None:
        """to_text() on an empty report should still include the header."""
        report = SurveyReport(repo_path="/empty")
        text = report.to_text()

        assert "Repository Survey: /empty" in text
        assert text.endswith("\n")

    def test_ends_with_newline(self) -> None:
        """to_text() output should end with exactly one newline."""
        report = SurveyReport(
            repo_path="/test",
            languages=["Python"],
            directory_markers=[
                DirectoryMarker(directory=".", marker_file="pyproject.toml"),
            ],
        )
        text = report.to_text()

        assert text.endswith("\n")
        assert not text.endswith("\n\n")


class TestSurveyReportToJson:
    """Tests for SurveyReport.to_json()."""

    def test_round_trips_all_fields(self) -> None:
        """to_json() should include all report fields."""
        report = SurveyReport(
            repo_path="/test/repo",
            languages=["Python", "Java"],
            package_managers=["Poetry"],
            frameworks=["FastAPI"],
            infrastructure=["Docker"],
            directory_markers=[
                DirectoryMarker(
                    directory="backend",
                    marker_file="pyproject.toml",
                    languages=["Python"],
                    package_managers=["Poetry"],
                    frameworks=["FastAPI"],
                ),
            ],
        )
        result = json.loads(report.to_json())

        assert result["repo_path"] == "/test/repo"
        assert result["languages"] == ["Python", "Java"]
        assert result["package_managers"] == ["Poetry"]
        assert result["frameworks"] == ["FastAPI"]
        assert result["infrastructure"] == ["Docker"]
        assert len(result["directory_markers"]) == 1
        assert result["directory_markers"][0]["directory"] == "backend"

    def test_empty_report(self) -> None:
        """to_json() should handle an empty report."""
        result = json.loads(SurveyReport(repo_path="/empty").to_json())

        assert result["repo_path"] == "/empty"
        assert result["languages"] == []
        assert result["directory_markers"] == []

    def test_respects_indent(self) -> None:
        """to_json() should respect the indent parameter."""
        report = SurveyReport(repo_path="/test")

        compact = report.to_json(indent=None)
        indented = report.to_json(indent=4)

        assert "\n" not in compact
        assert "\n" in indented
