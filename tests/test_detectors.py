"""Tests for technology detection functions."""

from pathlib import Path

import pytest

from repo_surveyor.constants import MarkerKey, TechCategory
from repo_surveyor.detectors import (
    detect_frameworks_for_file,
    detect_frameworks_from_packages,
    detect_from_glob_patterns,
    detect_from_indicator_files,
    detect_indicator_files_with_directories,
    detect_kubernetes,
    detect_languages_from_extensions,
)


class TestDetectFromIndicatorFiles:
    """Tests for detect_from_indicator_files (root-only)."""

    def test_detects_pyproject_toml(self, tmp_path: Path) -> None:
        """Should detect Python and Poetry from pyproject.toml."""
        (tmp_path / "pyproject.toml").write_text("[tool.poetry]\n")
        result = detect_from_indicator_files(tmp_path)

        assert "Python" in result[TechCategory.LANGUAGES]
        assert "Poetry" in result[TechCategory.PACKAGE_MANAGERS]

    def test_detects_package_json(self, tmp_path: Path) -> None:
        """Should detect JavaScript and npm from package.json."""
        (tmp_path / "package.json").write_text("{}\n")
        result = detect_from_indicator_files(tmp_path)

        assert "JavaScript" in result[TechCategory.LANGUAGES]
        assert "npm" in result[TechCategory.PACKAGE_MANAGERS]

    def test_detects_pom_xml(self, tmp_path: Path) -> None:
        """Should detect Java and Maven from pom.xml."""
        (tmp_path / "pom.xml").write_text("<project></project>\n")
        result = detect_from_indicator_files(tmp_path)

        assert "Java" in result[TechCategory.LANGUAGES]
        assert "Maven" in result[TechCategory.PACKAGE_MANAGERS]

    def test_detects_cargo_toml(self, tmp_path: Path) -> None:
        """Should detect Rust and Cargo from Cargo.toml."""
        (tmp_path / "Cargo.toml").write_text("[package]\n")
        result = detect_from_indicator_files(tmp_path)

        assert "Rust" in result[TechCategory.LANGUAGES]
        assert "Cargo" in result[TechCategory.PACKAGE_MANAGERS]

    def test_detects_go_mod(self, tmp_path: Path) -> None:
        """Should detect Go from go.mod."""
        (tmp_path / "go.mod").write_text("module example.com\n")
        result = detect_from_indicator_files(tmp_path)

        assert "Go" in result[TechCategory.LANGUAGES]

    def test_empty_directory(self, tmp_path: Path) -> None:
        """Should return empty sets for a directory with no indicator files."""
        result = detect_from_indicator_files(tmp_path)

        assert result[TechCategory.LANGUAGES] == set()
        assert result[TechCategory.PACKAGE_MANAGERS] == set()
        assert result[TechCategory.FRAMEWORKS] == set()
        assert result[TechCategory.INFRASTRUCTURE] == set()

    def test_multiple_indicators(self, tmp_path: Path) -> None:
        """Should detect all technologies from multiple indicator files."""
        (tmp_path / "pyproject.toml").write_text("[tool.poetry]\n")
        (tmp_path / "package.json").write_text("{}\n")
        result = detect_from_indicator_files(tmp_path)

        assert "Python" in result[TechCategory.LANGUAGES]
        assert "JavaScript" in result[TechCategory.LANGUAGES]


class TestDetectIndicatorFilesWithDirectories:
    """Tests for detect_indicator_files_with_directories."""

    def test_detects_root_indicator(self, tmp_path: Path) -> None:
        """Should detect indicator file at root with directory='.'."""
        (tmp_path / "pyproject.toml").write_text("[tool.poetry]\n")
        results = detect_indicator_files_with_directories(tmp_path)

        assert len(results) == 1
        assert results[0][MarkerKey.DIRECTORY] == "."
        assert results[0][MarkerKey.MARKER_FILE] == "pyproject.toml"
        assert "Python" in results[0][TechCategory.LANGUAGES]

    def test_detects_nested_indicator(self, tmp_path: Path) -> None:
        """Should detect indicator files in subdirectories."""
        backend = tmp_path / "backend"
        backend.mkdir()
        (backend / "pyproject.toml").write_text("[tool.poetry]\n")
        results = detect_indicator_files_with_directories(tmp_path)

        assert len(results) == 1
        assert results[0][MarkerKey.DIRECTORY] == "backend"

    def test_detects_multiple_directories(self, tmp_path: Path) -> None:
        """Should detect indicators across multiple directories."""
        (tmp_path / "pyproject.toml").write_text("[tool.poetry]\n")
        frontend = tmp_path / "frontend"
        frontend.mkdir()
        (frontend / "package.json").write_text("{}\n")
        results = detect_indicator_files_with_directories(tmp_path)

        dirs = {r[MarkerKey.DIRECTORY] for r in results}
        assert "." in dirs
        assert "frontend" in dirs

    def test_skips_default_dirs(self, tmp_path: Path) -> None:
        """Should skip node_modules and other default skip dirs."""
        nm = tmp_path / "node_modules" / "some-lib"
        nm.mkdir(parents=True)
        (nm / "package.json").write_text("{}\n")
        results = detect_indicator_files_with_directories(tmp_path)

        assert len(results) == 0

    def test_skips_extra_dirs(self, tmp_path: Path) -> None:
        """Should skip extra_skip_dirs."""
        vendor = tmp_path / "vendored"
        vendor.mkdir()
        (vendor / "pyproject.toml").write_text("[tool.poetry]\n")
        results = detect_indicator_files_with_directories(
            tmp_path, extra_skip_dirs=["vendored"]
        )

        assert len(results) == 0

    def test_skips_dotdirs(self, tmp_path: Path) -> None:
        """Should skip directories starting with a dot."""
        hidden = tmp_path / ".hidden"
        hidden.mkdir()
        (hidden / "package.json").write_text("{}\n")
        results = detect_indicator_files_with_directories(tmp_path)

        assert len(results) == 0

    def test_empty_directory(self, tmp_path: Path) -> None:
        """Should return empty list for a directory with no indicators."""
        results = detect_indicator_files_with_directories(tmp_path)

        assert results == []


class TestDetectFromGlobPatterns:
    """Tests for detect_from_glob_patterns."""

    def test_detects_csproj(self, tmp_path: Path) -> None:
        """Should detect C# from .csproj files."""
        (tmp_path / "MyApp.csproj").write_text(
            '<Project><ItemGroup><PackageReference Include="Newtonsoft.Json" /></ItemGroup></Project>'
        )
        result = detect_from_glob_patterns(tmp_path)

        assert "C#" in result[TechCategory.LANGUAGES]
        assert "NuGet" in result[TechCategory.PACKAGE_MANAGERS]

    def test_detects_terraform(self, tmp_path: Path) -> None:
        """Should detect Terraform from .tf files."""
        (tmp_path / "main.tf").write_text('resource "aws_instance" "web" {}\n')
        result = detect_from_glob_patterns(tmp_path)

        assert "Terraform" in result[TechCategory.INFRASTRUCTURE]

    def test_detects_nested_csproj(self, tmp_path: Path) -> None:
        """Should detect .csproj files in subdirectories via glob."""
        sub = tmp_path / "src" / "MyApp"
        sub.mkdir(parents=True)
        (sub / "MyApp.csproj").write_text("<Project></Project>")
        result = detect_from_glob_patterns(tmp_path)

        assert "C#" in result[TechCategory.LANGUAGES]

    def test_empty_directory(self, tmp_path: Path) -> None:
        """Should return empty sets for no glob matches."""
        result = detect_from_glob_patterns(tmp_path)

        assert result[TechCategory.LANGUAGES] == set()
        assert result[TechCategory.INFRASTRUCTURE] == set()

    def test_parses_frameworks_from_matched_files(self, tmp_path: Path) -> None:
        """Should parse matched files for framework dependencies."""
        (tmp_path / "MyApp.csproj").write_text(
            '<Project><ItemGroup>'
            '<PackageReference Include="Microsoft.AspNetCore.App" />'
            '</ItemGroup></Project>'
        )
        result = detect_from_glob_patterns(tmp_path)

        assert "ASP.NET Core" in result[TechCategory.FRAMEWORKS]


class TestDetectKubernetes:
    """Tests for detect_kubernetes."""

    def test_detects_k8s_deployment(self, tmp_path: Path) -> None:
        """Should detect Kubernetes from a deployment manifest."""
        (tmp_path / "deploy.yaml").write_text(
            "apiVersion: apps/v1\nkind: Deployment\n"
        )
        assert detect_kubernetes(tmp_path) is True

    def test_detects_k8s_in_yml(self, tmp_path: Path) -> None:
        """Should detect Kubernetes from .yml files too."""
        (tmp_path / "service.yml").write_text(
            "apiVersion: v1\nkind: Service\n"
        )
        assert detect_kubernetes(tmp_path) is True

    def test_no_k8s_in_regular_yaml(self, tmp_path: Path) -> None:
        """Should not detect Kubernetes in non-k8s YAML."""
        (tmp_path / "config.yaml").write_text("name: my-app\nversion: 1.0\n")
        assert detect_kubernetes(tmp_path) is False

    def test_empty_directory(self, tmp_path: Path) -> None:
        """Should return False for a directory with no YAML files."""
        assert detect_kubernetes(tmp_path) is False

    def test_detects_in_subdirectory(self, tmp_path: Path) -> None:
        """Should detect Kubernetes manifests in subdirectories."""
        k8s_dir = tmp_path / "k8s"
        k8s_dir.mkdir()
        (k8s_dir / "deployment.yaml").write_text(
            "apiVersion: apps/v1\nkind: Deployment\n"
        )
        assert detect_kubernetes(tmp_path) is True


class TestDetectLanguagesFromExtensions:
    """Tests for detect_languages_from_extensions."""

    def test_detects_python(self, tmp_path: Path) -> None:
        """Should detect Python from .py files."""
        (tmp_path / "main.py").write_text("print('hello')\n")
        result = detect_languages_from_extensions(tmp_path)

        assert "Python" in result

    def test_detects_multiple_languages(self, tmp_path: Path) -> None:
        """Should detect multiple languages from different extensions."""
        (tmp_path / "main.py").write_text("")
        (tmp_path / "App.java").write_text("")
        (tmp_path / "index.ts").write_text("")
        result = detect_languages_from_extensions(tmp_path)

        assert "Python" in result
        assert "Java" in result
        assert "TypeScript" in result

    def test_detects_in_subdirectories(self, tmp_path: Path) -> None:
        """Should detect languages in nested directories."""
        sub = tmp_path / "src" / "main"
        sub.mkdir(parents=True)
        (sub / "App.java").write_text("")
        result = detect_languages_from_extensions(tmp_path)

        assert "Java" in result

    def test_skips_node_modules(self, tmp_path: Path) -> None:
        """Should skip files in node_modules."""
        nm = tmp_path / "node_modules" / "lib"
        nm.mkdir(parents=True)
        (nm / "index.js").write_text("")
        result = detect_languages_from_extensions(tmp_path)

        assert "JavaScript" not in result

    def test_skips_dotdirs(self, tmp_path: Path) -> None:
        """Should skip hidden directories."""
        hidden = tmp_path / ".cache"
        hidden.mkdir()
        (hidden / "temp.py").write_text("")
        result = detect_languages_from_extensions(tmp_path)

        assert "Python" not in result

    def test_empty_directory(self, tmp_path: Path) -> None:
        """Should return empty set for directory with no source files."""
        result = detect_languages_from_extensions(tmp_path)

        assert result == set()

    def test_ignores_unknown_extensions(self, tmp_path: Path) -> None:
        """Should ignore files with unrecognised extensions."""
        (tmp_path / "data.csv").write_text("")
        (tmp_path / "notes.txt").write_text("")
        result = detect_languages_from_extensions(tmp_path)

        assert result == set()


class TestDetectFrameworksFromPackages:
    """Tests for detect_frameworks_from_packages."""

    def test_detects_fastapi_from_pyproject(self, tmp_path: Path) -> None:
        """Should detect FastAPI from pyproject.toml dependencies."""
        (tmp_path / "pyproject.toml").write_text(
            '[project]\ndependencies = ["fastapi"]\n'
        )
        result = detect_frameworks_from_packages(tmp_path)

        assert "FastAPI" in result

    def test_detects_spring_from_pom(self, tmp_path: Path) -> None:
        """Should detect Spring from pom.xml dependencies."""
        (tmp_path / "pom.xml").write_text(
            '<project xmlns="http://maven.apache.org/POM/4.0.0">'
            "<dependencies><dependency>"
            "<groupId>org.springframework.boot</groupId>"
            "<artifactId>spring-boot-starter-web</artifactId>"
            "</dependency></dependencies></project>"
        )
        result = detect_frameworks_from_packages(tmp_path)

        assert "Spring" in result

    def test_detects_express_from_package_json(self, tmp_path: Path) -> None:
        """Should detect Express from package.json."""
        (tmp_path / "package.json").write_text(
            '{"dependencies": {"express": "^4.18.0"}}'
        )
        result = detect_frameworks_from_packages(tmp_path)

        assert "Express" in result

    def test_empty_directory(self, tmp_path: Path) -> None:
        """Should return empty set for directory with no config files."""
        result = detect_frameworks_from_packages(tmp_path)

        assert result == set()


class TestDetectFrameworksForFile:
    """Tests for detect_frameworks_for_file."""

    def test_detects_frameworks_from_pyproject(self, tmp_path: Path) -> None:
        """Should detect frameworks from a specific pyproject.toml."""
        filepath = tmp_path / "pyproject.toml"
        filepath.write_text('[project]\ndependencies = ["django"]\n')
        result = detect_frameworks_for_file(filepath)

        assert "Django" in result

    def test_returns_empty_for_nonexistent_file(self, tmp_path: Path) -> None:
        """Should return empty list for a file that doesn't exist."""
        result = detect_frameworks_for_file(tmp_path / "nonexistent.toml")

        assert result == []

    def test_returns_empty_for_no_frameworks(self, tmp_path: Path) -> None:
        """Should return empty list when no frameworks are detected."""
        filepath = tmp_path / "pyproject.toml"
        filepath.write_text('[project]\ndependencies = ["some-unknown-lib"]\n')
        result = detect_frameworks_for_file(filepath)

        assert result == []

    def test_detects_react_from_package_json(self, tmp_path: Path) -> None:
        """Should detect React from package.json."""
        filepath = tmp_path / "package.json"
        filepath.write_text('{"dependencies": {"react": "^18.0.0"}}')
        result = detect_frameworks_for_file(filepath)

        assert "React" in result
