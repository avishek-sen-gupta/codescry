"""Tests for AnalysisGraphBuilder class."""

from pathlib import Path
from unittest.mock import MagicMock

import pytest

from repo_surveyor.analysis_graph_builder import AnalysisGraphBuilder
from repo_surveyor.report import DirectoryMarker, SurveyReport
from repo_surveyor.ctags import CTagsEntry, CTagsResult
from repo_surveyor.surveyor import RepoSurveyor
from repo_surveyor.graph_builder import (
    build_tech_stack_graph,
    build_coarse_structure_graph,
    extract_package,
)


def create_mock_driver() -> MagicMock:
    """Create a mock Neo4j driver for testing."""
    mock_driver = MagicMock()
    mock_session = MagicMock()
    mock_driver.session.return_value.__enter__ = MagicMock(return_value=mock_session)
    mock_driver.session.return_value.__exit__ = MagicMock(return_value=False)
    return mock_driver


def create_mock_driver_with_session() -> tuple[MagicMock, MagicMock]:
    """Create a mock Neo4j driver and return both driver and session."""
    mock_driver = MagicMock()
    mock_session = MagicMock()
    mock_driver.session.return_value.__enter__ = MagicMock(return_value=mock_session)
    mock_driver.session.return_value.__exit__ = MagicMock(return_value=False)
    return mock_driver, mock_session


class TestAnalysisGraphBuilderInit:
    """Test AnalysisGraphBuilder initialization and connection management."""

    def test_init_stores_driver(self) -> None:
        """Should store the injected driver."""
        mock_driver = create_mock_driver()
        persistence = AnalysisGraphBuilder(mock_driver)

        assert persistence.driver is mock_driver

    def test_close_closes_driver(self) -> None:
        """Should close the driver connection."""
        mock_driver = create_mock_driver()
        persistence = AnalysisGraphBuilder(mock_driver)

        persistence.close()

        mock_driver.close.assert_called_once()

    def test_context_manager_closes_on_exit(self) -> None:
        """Should close driver when exiting context manager."""
        mock_driver = create_mock_driver()

        with AnalysisGraphBuilder(mock_driver) as persistence:
            assert persistence is not None

        mock_driver.close.assert_called_once()

    def test_context_manager_returns_self(self) -> None:
        """Should return self when entering context manager."""
        mock_driver = create_mock_driver()

        with AnalysisGraphBuilder(mock_driver) as persistence:
            assert isinstance(persistence, AnalysisGraphBuilder)


class TestExtractPackage:
    """Test extract_package function for package name extraction."""

    def test_extract_java_package_from_standard_path(self) -> None:
        """Should extract Java package from src/main/java path."""
        package = extract_package(
            "src/main/java/com/example/utils/Helper.java",
            "Java",
        )
        assert package == "com.example.utils"

    def test_extract_java_package_from_test_path(self) -> None:
        """Should extract Java package from src/test/java path."""
        package = extract_package(
            "src/test/java/com/example/tests/HelperTest.java",
            "Java",
        )
        assert package == "com.example.tests"

    def test_extract_java_package_from_src_path(self) -> None:
        """Should extract Java package from src/ path."""
        package = extract_package(
            "src/com/example/Main.java",
            "Java",
        )
        assert package == "com.example"

    def test_extract_java_package_fallback(self) -> None:
        """Should use full directory path as fallback for Java."""
        package = extract_package(
            "java/com/example/Service.java",
            "Java",
        )
        assert package == "java.com.example"

    def test_extract_python_package_from_src(self) -> None:
        """Should extract Python package from src/ path."""
        package = extract_package(
            "src/mypackage/utils/helpers.py",
            "Python",
        )
        assert package == "mypackage.utils"

    def test_extract_python_package_from_lib(self) -> None:
        """Should extract Python package from lib/ path."""
        package = extract_package(
            "lib/mypackage/core.py",
            "Python",
        )
        assert package == "mypackage"

    def test_extract_python_package_fallback(self) -> None:
        """Should use full directory path as fallback for Python."""
        package = extract_package(
            "mypackage/utils/helpers.py",
            "Python",
        )
        assert package == "mypackage.utils"

    def test_extract_csharp_package(self) -> None:
        """Should extract C# namespace from directory path."""
        package = extract_package(
            "MyProject/Utils/Helper.cs",
            "C#",
        )
        assert package == "MyProject.Utils"

    def test_extract_csharp_package_alternate(self) -> None:
        """Should handle csharp as language name."""
        package = extract_package(
            "MyProject/Services/Service.cs",
            "csharp",
        )
        assert package == "MyProject.Services"

    def test_extract_package_returns_empty_for_no_language(self) -> None:
        """Should return empty string when language is None."""
        package = extract_package("src/file.txt", None)
        assert package == ""

    def test_extract_package_returns_empty_for_no_directory(self) -> None:
        """Should return empty string when file has no directory."""
        package = extract_package("Main.java", "Java")
        assert package == ""

    def test_extract_package_default_language(self) -> None:
        """Should use directory path for unknown languages."""
        package = extract_package(
            "mymodule/components/widget.go",
            "Go",
        )
        assert package == "mymodule.components"


class TestBuildTechStackGraph:
    """Test build_tech_stack_graph function."""

    def test_empty_report(self) -> None:
        """Should handle empty report with no directory markers."""
        report = SurveyReport(
            repo_path="/test/repo",
            languages=[],
            package_managers=[],
            frameworks=[],
            infrastructure=[],
            directory_markers=[],
        )

        directories, relationships, tech_nodes, top_level = build_tech_stack_graph(
            report
        )

        assert directories == []
        assert relationships == []
        assert tech_nodes == []
        assert top_level == set()

    def test_single_directory_marker(self) -> None:
        """Should process single directory marker correctly."""
        report = SurveyReport(
            repo_path="/test/repo",
            languages=["Java"],
            package_managers=["Maven"],
            frameworks=[],
            infrastructure=[],
            directory_markers=[
                DirectoryMarker(
                    directory="src",
                    marker_file="pom.xml",
                    languages=["Java"],
                    package_managers=["Maven"],
                    frameworks=[],
                    infrastructure=[],
                )
            ],
        )

        directories, relationships, tech_nodes, top_level = build_tech_stack_graph(
            report
        )

        assert len(directories) == 1
        assert directories[0]["path"] == "src"
        assert directories[0]["name"] == "src"
        assert directories[0]["marker_file"] == "pom.xml"
        assert relationships == []
        assert len(tech_nodes) == 2  # Java + Maven
        assert "src" in top_level

    def test_nested_directory_creates_hierarchy(self) -> None:
        """Should create parent directories and relationships for nested paths."""
        report = SurveyReport(
            repo_path="/test/repo",
            languages=["Python"],
            package_managers=[],
            frameworks=[],
            infrastructure=[],
            directory_markers=[
                DirectoryMarker(
                    directory="src/mypackage/utils",
                    marker_file="__init__.py",
                    languages=["Python"],
                    package_managers=[],
                    frameworks=[],
                    infrastructure=[],
                )
            ],
        )

        directories, relationships, tech_nodes, top_level = build_tech_stack_graph(
            report
        )

        # Should have 3 directories: src, src/mypackage, src/mypackage/utils
        dir_paths = [d["path"] for d in directories]
        assert "src" in dir_paths
        assert "src/mypackage" in dir_paths
        assert "src/mypackage/utils" in dir_paths

        # Should have 2 relationships
        assert len(relationships) == 2
        assert {"parent": "src", "child": "src/mypackage"} in relationships
        assert {
            "parent": "src/mypackage",
            "child": "src/mypackage/utils",
        } in relationships

        # Top level should be "src"
        assert top_level == {"src"}

    def test_skips_root_directory(self) -> None:
        """Should skip root directory marker."""
        report = SurveyReport(
            repo_path="/test/repo",
            languages=[],
            package_managers=[],
            frameworks=[],
            infrastructure=[],
            directory_markers=[
                DirectoryMarker(
                    directory=".",
                    marker_file="package.json",
                    languages=["JavaScript"],
                    package_managers=["npm"],
                    frameworks=[],
                    infrastructure=[],
                )
            ],
        )

        directories, relationships, tech_nodes, top_level = build_tech_stack_graph(
            report
        )

        assert directories == []
        assert tech_nodes == []

    def test_technology_nodes_created(self) -> None:
        """Should create technology nodes for all detected technologies."""
        report = SurveyReport(
            repo_path="/test/repo",
            languages=["TypeScript"],
            package_managers=["npm"],
            frameworks=["React"],
            infrastructure=["Docker"],
            directory_markers=[
                DirectoryMarker(
                    directory="frontend",
                    marker_file="package.json",
                    languages=["TypeScript"],
                    package_managers=["npm"],
                    frameworks=["React"],
                    infrastructure=["Docker"],
                )
            ],
        )

        directories, relationships, tech_nodes, top_level = build_tech_stack_graph(
            report
        )

        assert len(tech_nodes) == 4

        types = [n["type"] for n in tech_nodes]
        assert "Language" in types
        assert "PackageManager" in types
        assert "Framework" in types
        assert "Infrastructure" in types

        names = [n["name"] for n in tech_nodes]
        assert "TypeScript" in names
        assert "npm" in names
        assert "React" in names
        assert "Docker" in names

    def test_multiple_markers_deduplicate_directories(self) -> None:
        """Should deduplicate directories when multiple markers exist."""
        report = SurveyReport(
            repo_path="/test/repo",
            languages=["Java", "Python"],
            package_managers=[],
            frameworks=[],
            infrastructure=[],
            directory_markers=[
                DirectoryMarker(
                    directory="src",
                    marker_file="pom.xml",
                    languages=["Java"],
                    package_managers=[],
                    frameworks=[],
                    infrastructure=[],
                ),
                DirectoryMarker(
                    directory="src",
                    marker_file="requirements.txt",
                    languages=["Python"],
                    package_managers=[],
                    frameworks=[],
                    infrastructure=[],
                ),
            ],
        )

        directories, relationships, tech_nodes, top_level = build_tech_stack_graph(
            report
        )

        # Should only have one directory entry
        assert len(directories) == 1
        assert directories[0]["path"] == "src"
        # marker_file should be from first marker (pom.xml)
        assert directories[0]["marker_file"] == "pom.xml"


class TestBuildCoarseStructureGraph:
    """Test build_coarse_structure_graph function."""

    def test_empty_result(self) -> None:
        """Should handle empty CTags result."""
        result = CTagsResult(entries=[], raw_output="", return_code=0)

        symbols, relationships, top_level = build_coarse_structure_graph(result)

        assert symbols == []
        assert relationships == []
        assert top_level == set()

    def test_single_class_symbol(self) -> None:
        """Should process single class symbol."""
        result = CTagsResult(
            entries=[
                CTagsEntry(
                    name="MyClass",
                    path="src/main/java/com/example/MyClass.java",
                    kind="class",
                    line=10,
                    end=None,
                    scope=None,
                    scope_kind=None,
                    signature=None,
                    language="Java",
                )
            ],
            raw_output="",
            return_code=0,
        )

        symbols, relationships, top_level = build_coarse_structure_graph(result)

        assert len(symbols) == 1
        assert symbols[0]["name"] == "MyClass"
        assert symbols[0]["kind"] == "class"
        assert symbols[0]["package"] == "com.example"
        assert len(top_level) == 1  # Class is top-level (no parent)

    def test_method_with_class_scope(self) -> None:
        """Should create parent-child relationship for method in class."""
        result = CTagsResult(
            entries=[
                CTagsEntry(
                    name="MyClass",
                    path="src/MyClass.java",
                    kind="class",
                    line=1,
                    end=None,
                    scope=None,
                    scope_kind=None,
                    signature=None,
                    language="Java",
                ),
                CTagsEntry(
                    name="myMethod",
                    path="src/MyClass.java",
                    kind="method",
                    line=5,
                    end=None,
                    scope="MyClass",
                    scope_kind="class",
                    signature="()",
                    language="Java",
                ),
            ],
            raw_output="",
            return_code=0,
        )

        symbols, relationships, top_level = build_coarse_structure_graph(result)

        assert len(symbols) == 2
        assert len(relationships) == 1
        assert len(top_level) == 1  # Only class is top-level

        # Verify relationship
        rel = relationships[0]
        assert "myMethod" in rel["child_id"]
        assert "MyClass" in rel["parent_id"]

    def test_unique_symbol_id_format(self) -> None:
        """Should create unique symbol IDs with path:name:kind:line format."""
        result = CTagsResult(
            entries=[
                CTagsEntry(
                    name="getValue",
                    path="src/Foo.java",
                    kind="method",
                    line=25,
                    end=None,
                    scope="Foo",
                    scope_kind="class",
                    signature="()",
                    language="Java",
                )
            ],
            raw_output="",
            return_code=0,
        )

        symbols, relationships, top_level = build_coarse_structure_graph(result)

        assert symbols[0]["id"] == "src/Foo.java:getValue:method:25"

    def test_handles_overloaded_methods(self) -> None:
        """Should handle overloaded methods with different line numbers."""
        result = CTagsResult(
            entries=[
                CTagsEntry(
                    name="process",
                    path="src/Service.java",
                    kind="method",
                    line=10,
                    end=None,
                    scope=None,
                    scope_kind=None,
                    signature="(String)",
                    language="Java",
                ),
                CTagsEntry(
                    name="process",
                    path="src/Service.java",
                    kind="method",
                    line=20,
                    end=None,
                    scope=None,
                    scope_kind=None,
                    signature="(int)",
                    language="Java",
                ),
            ],
            raw_output="",
            return_code=0,
        )

        symbols, relationships, top_level = build_coarse_structure_graph(result)

        # Should have 2 distinct symbols
        assert len(symbols) == 2
        ids = [s["id"] for s in symbols]
        assert "src/Service.java:process:method:10" in ids
        assert "src/Service.java:process:method:20" in ids

    def test_scope_kind_filtering(self) -> None:
        """Should filter parent matching by scope_kind."""
        result = CTagsResult(
            entries=[
                CTagsEntry(
                    name="Config",
                    path="src/Config.java",
                    kind="class",
                    line=1,
                    end=None,
                    scope=None,
                    scope_kind=None,
                    signature=None,
                    language="Java",
                ),
                CTagsEntry(
                    name="Config",
                    path="src/Config.java",
                    kind="interface",
                    line=50,
                    end=None,
                    scope=None,
                    scope_kind=None,
                    signature=None,
                    language="Java",
                ),
                CTagsEntry(
                    name="getValue",
                    path="src/Config.java",
                    kind="method",
                    line=10,
                    end=None,
                    scope="Config",
                    scope_kind="class",
                    signature="()",
                    language="Java",
                ),
            ],
            raw_output="",
            return_code=0,
        )

        symbols, relationships, top_level = build_coarse_structure_graph(result)

        # Method should only link to class Config, not interface Config
        assert len(relationships) == 1
        rel = relationships[0]
        assert "Config:class:1" in rel["parent_id"]


class TestPersistTechStacks:
    """Test persist_tech_stacks method."""

    def test_creates_repository_node(self) -> None:
        """Should create Repository node with path and name."""
        mock_driver, mock_session = create_mock_driver_with_session()
        persistence = AnalysisGraphBuilder(mock_driver)

        report = SurveyReport(
            repo_path="/home/user/myproject",
            languages=[],
            package_managers=[],
            frameworks=[],
            infrastructure=[],
            directory_markers=[],
        )

        persistence.persist_tech_stacks(report)

        # Find the Repository creation call
        calls = mock_session.run.call_args_list
        repo_call = [c for c in calls if "Repository" in str(c)]
        assert len(repo_call) >= 1

    def test_creates_directory_nodes(self) -> None:
        """Should create Directory nodes for markers."""
        mock_driver, mock_session = create_mock_driver_with_session()
        persistence = AnalysisGraphBuilder(mock_driver)

        report = SurveyReport(
            repo_path="/test/repo",
            languages=["Java"],
            package_managers=[],
            frameworks=[],
            infrastructure=[],
            directory_markers=[
                DirectoryMarker(
                    directory="src/main",
                    marker_file="pom.xml",
                    languages=["Java"],
                    package_managers=[],
                    frameworks=[],
                    infrastructure=[],
                )
            ],
        )

        persistence.persist_tech_stacks(report)

        # Verify session.run was called with directory creation
        calls = mock_session.run.call_args_list
        dir_calls = [c for c in calls if "Directory" in str(c)]
        assert len(dir_calls) >= 1


class TestPersistCoarseStructure:
    """Test persist_coarse_structure method."""

    def test_raises_on_failed_ctags_result(self) -> None:
        """Should raise ValueError when CTags failed."""
        mock_driver = create_mock_driver()
        persistence = AnalysisGraphBuilder(mock_driver)

        result = CTagsResult(
            entries=[],
            raw_output="",
            return_code=1,
            error_output="ctags: error parsing file",
        )

        with pytest.raises(ValueError, match="CTags failed"):
            persistence.persist_coarse_structure(result, "/test/repo")

    def test_handles_empty_symbols(self) -> None:
        """Should handle empty symbols list gracefully."""
        mock_driver, mock_session = create_mock_driver_with_session()
        persistence = AnalysisGraphBuilder(mock_driver)

        result = CTagsResult(entries=[], raw_output="", return_code=0)

        persistence.persist_coarse_structure(result, "/test/repo")

        # Should not create any nodes
        mock_session.run.assert_not_called()

    def test_creates_code_symbol_nodes(self) -> None:
        """Should create CodeSymbol nodes for entries."""
        mock_driver, mock_session = create_mock_driver_with_session()
        persistence = AnalysisGraphBuilder(mock_driver)

        result = CTagsResult(
            entries=[
                CTagsEntry(
                    name="MyClass",
                    path="src/MyClass.java",
                    kind="class",
                    line=1,
                    end=None,
                    scope=None,
                    scope_kind=None,
                    signature=None,
                    language="Java",
                )
            ],
            raw_output="",
            return_code=0,
        )

        persistence.persist_coarse_structure(result, "/test/repo")

        # Verify CodeSymbol creation was called
        calls = mock_session.run.call_args_list
        symbol_calls = [c for c in calls if "CodeSymbol" in str(c)]
        assert len(symbol_calls) >= 1


class TestRepoSurveyorAdditional:
    """Additional tests for RepoSurveyor class."""

    def test_init_with_nonexistent_path(self) -> None:
        """Should raise ValueError for nonexistent path."""
        with pytest.raises(ValueError, match="Path does not exist"):
            RepoSurveyor("/nonexistent/path/that/does/not/exist")

    def test_init_with_file_path(self, tmp_path: Path) -> None:
        """Should raise ValueError when path is a file, not directory."""
        file_path = tmp_path / "testfile.txt"
        file_path.touch()

        with pytest.raises(ValueError, match="not a directory"):
            RepoSurveyor(str(file_path))

    def test_init_resolves_path(self, tmp_path: Path) -> None:
        """Should resolve relative paths to absolute."""
        surveyor = RepoSurveyor(str(tmp_path))
        assert surveyor.repo_path.is_absolute()

    def test_tech_stacks_returns_survey_report(self, tmp_path: Path) -> None:
        """Should return SurveyReport instance."""
        surveyor = RepoSurveyor(str(tmp_path))
        report = surveyor.tech_stacks()

        assert isinstance(report, SurveyReport)
        assert report.repo_path == str(tmp_path)

    def test_tech_stacks_on_empty_directory(self, tmp_path: Path) -> None:
        """Should handle empty directory gracefully."""
        surveyor = RepoSurveyor(str(tmp_path))
        report = surveyor.tech_stacks()

        assert report.languages == []
        assert report.package_managers == []
        assert report.frameworks == []
        assert report.infrastructure == []

    def test_coarse_structure_default_excludes(self, tmp_path: Path) -> None:
        """Should use default exclude patterns when none specified."""
        surveyor = RepoSurveyor(str(tmp_path))

        # Note: This will fail if ctags is not installed
        try:
            result = surveyor.coarse_structure()
            # Default excludes should be applied
            assert result is not None
        except FileNotFoundError:
            pytest.skip("ctags not installed")

    def test_coarse_structure_custom_languages(self, tmp_path: Path) -> None:
        """Should accept custom language filter."""
        surveyor = RepoSurveyor(str(tmp_path))

        try:
            result = surveyor.coarse_structure(languages=["Python", "Java"])
            assert result is not None
        except FileNotFoundError:
            pytest.skip("ctags not installed")

    def test_coarse_structure_custom_exclude_patterns(self, tmp_path: Path) -> None:
        """Should accept custom exclude patterns."""
        surveyor = RepoSurveyor(str(tmp_path))

        try:
            result = surveyor.coarse_structure(
                exclude_patterns=["build", "dist", "coverage"]
            )
            assert result is not None
        except FileNotFoundError:
            pytest.skip("ctags not installed")
