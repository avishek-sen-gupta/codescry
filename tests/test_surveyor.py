"""Tests for the repo surveyor."""

import os
import sys
from pathlib import Path

import pytest

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from repo_surveyor import CTagsConfig, CTagsEntry, RepoSurveyor
from repo_surveyor.ctags import _build_ctags_command, _parse_ctags_json_output


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
        report = surveyor.tech_stacks()

        assert "TypeScript" in report.languages
        assert "JavaScript" in report.languages

    def test_detects_package_managers(self, mojo_lsp_path: Path) -> None:
        """Should detect npm from package.json."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.tech_stacks()

        assert "npm" in report.package_managers

    def test_frameworks_detection(self, mojo_lsp_path: Path) -> None:
        """Should not detect web frameworks (this is an LSP library)."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.tech_stacks()

        # mojo-lsp is an LSP client library, not a web app
        # It uses vscode-languageserver-protocol, not React/Vue/etc.
        assert report.frameworks == []

    def test_report_generation(self, mojo_lsp_path: Path) -> None:
        """Should generate a valid text report."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.tech_stacks()
        text = report.to_text()

        assert "Repository Survey:" in text
        assert "Languages:" in text
        assert "TypeScript" in text

    def test_directory_markers(self, mojo_lsp_path: Path) -> None:
        """Should detect directory markers for package.json and tsconfig.json."""
        surveyor = RepoSurveyor(str(mojo_lsp_path))
        report = surveyor.tech_stacks()

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
        report = surveyor.tech_stacks()
        text = report.to_text()

        assert "Directory Markers:" in text
        assert "Marker: package.json" in text
        assert "Marker: tsconfig.json" in text

    def test_random_repo(self):
        surveyor = RepoSurveyor("/Users/asgupta/code/smojol")
        survey = surveyor.tech_stacks()
        print(survey.to_text())


class TestCTagsCommandBuilding:
    """Test CTags command building."""

    def test_build_basic_command(self) -> None:
        """Should build a basic ctags command with defaults."""
        config = CTagsConfig()
        cmd = _build_ctags_command(config)

        assert cmd[0] == "ctags"
        assert "--output-format=json" in cmd
        assert "-R" in cmd
        assert "--fields=+n+k+S+z+K+l" in cmd
        assert "--extras=+q" in cmd

    def test_build_command_with_languages(self) -> None:
        """Should include language filter when specified."""
        config = CTagsConfig(languages=["Java", "Python"])
        cmd = _build_ctags_command(config)

        assert "--languages=Java,Python" in cmd

    def test_build_command_with_exclude_patterns(self) -> None:
        """Should include exclude patterns."""
        config = CTagsConfig(exclude_patterns=[".idea", "target", "resources"])
        cmd = _build_ctags_command(config)

        assert "--exclude=.idea" in cmd
        assert "--exclude=target" in cmd
        assert "--exclude=resources" in cmd

    def test_build_command_with_verbose(self) -> None:
        """Should include verbose flag when enabled."""
        config = CTagsConfig(verbose=True)
        cmd = _build_ctags_command(config)

        assert "--verbose=yes" in cmd

    def test_build_command_without_verbose(self) -> None:
        """Should not include verbose flag when disabled."""
        config = CTagsConfig(verbose=False)
        cmd = _build_ctags_command(config)

        assert "--verbose=yes" not in cmd


class TestCTagsJsonParsing:
    """Test parsing CTags JSON output."""

    def test_parse_single_entry(self) -> None:
        """Should parse a single CTags JSON entry."""
        output = '{"_type": "tag", "name": "MyClass", "path": "src/MyClass.java", "kind": "class", "line": 10, "language": "Java"}'
        entries = _parse_ctags_json_output(output)

        assert len(entries) == 1
        assert entries[0].name == "MyClass"
        assert entries[0].path == "src/MyClass.java"
        assert entries[0].kind == "class"
        assert entries[0].line == 10
        assert entries[0].language == "Java"

    def test_parse_multiple_entries(self) -> None:
        """Should parse multiple CTags JSON entries."""
        output = """{"_type": "tag", "name": "ClassA", "path": "a.java", "kind": "class", "line": 1}
{"_type": "tag", "name": "methodB", "path": "a.java", "kind": "method", "line": 5}
{"_type": "tag", "name": "fieldC", "path": "a.java", "kind": "field", "line": 3}"""
        entries = _parse_ctags_json_output(output)

        assert len(entries) == 3
        assert entries[0].name == "ClassA"
        assert entries[1].name == "methodB"
        assert entries[2].name == "fieldC"

    def test_parse_entry_with_scope(self) -> None:
        """Should parse entries with scope information."""
        output = '{"_type": "tag", "name": "getValue", "path": "src/Foo.java", "kind": "method", "line": 20, "scope": "Foo", "scopeKind": "class", "signature": "()"}'
        entries = _parse_ctags_json_output(output)

        assert len(entries) == 1
        assert entries[0].scope == "Foo"
        assert entries[0].scope_kind == "class"
        assert entries[0].signature == "()"

    def test_parse_ignores_non_tag_entries(self) -> None:
        """Should ignore non-tag entries like ptag entries."""
        output = """{"_type": "ptag", "name": "JSON_OUTPUT_VERSION", "parserName": "CTagsSelfTest"}
{"_type": "tag", "name": "MyClass", "path": "src/MyClass.java", "kind": "class", "line": 10}"""
        entries = _parse_ctags_json_output(output)

        assert len(entries) == 1
        assert entries[0].name == "MyClass"

    def test_parse_handles_empty_output(self) -> None:
        """Should handle empty output."""
        entries = _parse_ctags_json_output("")
        assert entries == []

    def test_parse_handles_malformed_json(self) -> None:
        """Should skip malformed JSON lines."""
        output = """{"_type": "tag", "name": "Good", "path": "a.java", "kind": "class", "line": 1}
not valid json
{"_type": "tag", "name": "AlsoGood", "path": "b.java", "kind": "class", "line": 2}"""
        entries = _parse_ctags_json_output(output)

        assert len(entries) == 2
        assert entries[0].name == "Good"
        assert entries[1].name == "AlsoGood"


class TestCTagsEntry:
    """Test CTagsEntry dataclass."""

    def test_from_json_minimal(self) -> None:
        """Should create entry from minimal JSON data."""
        data = {"name": "test", "path": "test.py", "kind": "function"}
        entry = CTagsEntry.from_json(data)

        assert entry.name == "test"
        assert entry.path == "test.py"
        assert entry.kind == "function"
        assert entry.line is None
        assert entry.scope is None

    def test_from_json_complete(self) -> None:
        """Should create entry from complete JSON data."""
        data = {
            "name": "MyMethod",
            "path": "src/MyClass.java",
            "kind": "method",
            "line": 25,
            "scope": "MyClass",
            "scopeKind": "class",
            "signature": "(int x, String y)",
            "language": "Java",
        }
        entry = CTagsEntry.from_json(data)

        assert entry.name == "MyMethod"
        assert entry.path == "src/MyClass.java"
        assert entry.kind == "method"
        assert entry.line == 25
        assert entry.scope == "MyClass"
        assert entry.scope_kind == "class"
        assert entry.signature == "(int x, String y)"
        assert entry.language == "Java"


class TestRepoSurveyorCTags:
    """Test RepoSurveyor CTags integration."""

    def test_run_ctags_on_java_repo(self) -> None:
        """Should run CTags on a Java repository."""
        surveyor = RepoSurveyor("/Users/asgupta/code/smojol")
        result = surveyor.coarse_structure(languages=["Java"])

        assert result.success
        assert len(result.entries) > 0
        class_entries = [e for e in result.entries if e.kind == "class"]
        assert len(class_entries) > 0

    def test_run_ctags_with_custom_excludes(self) -> None:
        """Should respect custom exclude patterns."""
        surveyor = RepoSurveyor("/Users/asgupta/code/smojol")
        result = surveyor.coarse_structure(
            languages=["Java"],
            exclude_patterns=[".git", ".idea", "target", "resources", "test"],
        )

        assert result.success
        for entry in result.entries:
            assert "/target/" not in entry.path
            assert "/.idea/" not in entry.path

    def test_run_ctags_returns_methods_with_signatures(self) -> None:
        """Should extract method signatures."""
        surveyor = RepoSurveyor("/Users/asgupta/code/smojol/smojol-common")
        result = surveyor.coarse_structure(languages=["Java"])

        assert result.success
        method_entries = [e for e in result.entries if e.kind == "method"]
        methods_with_sig = [e for e in method_entries if e.signature]
        assert len(methods_with_sig) > 0
