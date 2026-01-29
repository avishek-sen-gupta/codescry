"""Tests for the repo surveyor."""

import os
import sys
from pathlib import Path

import pytest

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from repo_surveyor import RepoSurveyor


class TestMojoLspDetection:
    """Test framework detection on ~/code/mojo-lsp."""

    @pytest.fixture
    def mojo_lsp_path(self) -> Path:
        """Return the path to mojo-lsp repo."""
        path = Path(os.path.expanduser("~/code/mojo-lsp"))
        return path

    def test_detects_languages(self, mojo_lsp_path: Path) -> None:
        """Should detect TypeScript and JavaScript."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.survey()

        assert "TypeScript" in report.languages
        assert "JavaScript" in report.languages

    def test_detects_package_managers(self, mojo_lsp_path: Path) -> None:
        """Should detect npm from package.json."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.survey()

        assert "npm" in report.package_managers

    def test_frameworks_detection(self, mojo_lsp_path: Path) -> None:
        """Should not detect web frameworks (this is an LSP library)."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.survey()

        # mojo-lsp is an LSP client library, not a web app
        # It uses vscode-languageserver-protocol, not React/Vue/etc.
        assert report.frameworks == []

    def test_report_generation(self, mojo_lsp_path: Path) -> None:
        """Should generate a valid text report."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.survey()
        text = report.to_text()

        assert "Repository Survey:" in text
        assert "Languages:" in text
        assert "TypeScript" in text

    def test_directory_markers(self, mojo_lsp_path: Path) -> None:
        """Should detect directory markers for package.json and tsconfig.json."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.survey()

        # Should have directory markers
        assert len(report.directory_markers) >= 2

        # Find the package.json marker
        package_markers = [m for m in report.directory_markers if m.marker_file == "package.json"]
        assert len(package_markers) == 1
        assert package_markers[0].directory == "."
        assert "JavaScript" in package_markers[0].languages
        assert "npm" in package_markers[0].package_managers

        # Find the tsconfig.json marker
        tsconfig_markers = [m for m in report.directory_markers if m.marker_file == "tsconfig.json"]
        assert len(tsconfig_markers) == 1
        assert tsconfig_markers[0].directory == "."
        assert "TypeScript" in tsconfig_markers[0].languages

    def test_directory_markers_in_report_text(self, mojo_lsp_path: Path) -> None:
        """Should include directory markers in the text report."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.survey()
        text = report.to_text()

        assert "Directory Markers:" in text
        assert "Marker: package.json" in text
        assert "Marker: tsconfig.json" in text
