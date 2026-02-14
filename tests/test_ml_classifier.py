"""Integration tests for ml_classifier using a StubClassifierModel."""

from pathlib import Path

from repo_surveyor.ml_classifier.classifier import classify_file, classify_repository
from repo_surveyor.ml_classifier.types import (
    CompletionResult,
    MLIntegrationType,
)


class StubClassifierModel:
    """Stub model that echoes back NONE for all lines."""

    @property
    def model_id(self) -> str:
        return "stub-model-v1"

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        lines = []
        for line in user_prompt.strip().splitlines():
            parts = line.split("|", maxsplit=1)
            if len(parts) == 2:
                line_num = parts[0].strip()
                lines.append(f"{line_num}|none")
        return CompletionResult(
            text="\n".join(lines),
            prompt_tokens=100,
            completion_tokens=50,
        )


class HttpStubModel:
    """Stub model that classifies lines containing 'requests' as http_client."""

    @property
    def model_id(self) -> str:
        return "http-stub-v1"

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        lines = []
        for line in user_prompt.strip().splitlines():
            parts = line.split("|", maxsplit=1)
            if len(parts) == 2:
                line_num = parts[0].strip()
                code = parts[1]
                if "requests" in code.lower():
                    lines.append(f"{line_num}|http_client")
                else:
                    lines.append(f"{line_num}|none")
        return CompletionResult(
            text="\n".join(lines),
            prompt_tokens=100,
            completion_tokens=50,
        )


class TestClassifyFile:
    def test_simple_file(self, tmp_path: Path):
        src = tmp_path / "app.py"
        src.write_text("import os\nx = 1\nprint(x)\n")

        result = classify_file(src, StubClassifierModel())

        assert result is not None
        assert result.file_path == str(src)
        assert result.language == "Python"
        assert result.lines_submitted == 3
        assert result.lines_skipped == 0
        assert len(result.classified_lines) == 3
        assert all(
            cl.integration_type == MLIntegrationType.NONE
            for cl in result.classified_lines
        )

    def test_file_with_trivial_lines(self, tmp_path: Path):
        src = tmp_path / "app.py"
        src.write_text("import os\n\n\nx = 1\n  \n")

        result = classify_file(src, StubClassifierModel())

        assert result is not None
        assert result.lines_submitted == 2
        assert result.lines_skipped == 3

    def test_all_trivial_lines(self, tmp_path: Path):
        src = tmp_path / "empty.py"
        src.write_text("\n\n  \n{\n}\n")

        result = classify_file(src, StubClassifierModel())

        assert result is not None
        assert result.lines_submitted == 0
        assert result.lines_skipped == 5
        assert result.classified_lines == ()

    def test_http_detection(self, tmp_path: Path):
        src = tmp_path / "client.py"
        src.write_text("import requests\nresponse = requests.get(url)\nx = 1\n")

        result = classify_file(src, HttpStubModel())

        assert result is not None
        http_lines = [
            cl
            for cl in result.classified_lines
            if cl.integration_type == MLIntegrationType.HTTP_CLIENT
        ]
        assert len(http_lines) == 2
        assert http_lines[0].line_number == 1
        assert http_lines[1].line_number == 2

    def test_unreadable_file(self, tmp_path: Path):
        src = tmp_path / "missing.py"
        result = classify_file(src, StubClassifierModel())
        assert result is None

    def test_unknown_language(self, tmp_path: Path):
        src = tmp_path / "data.xyz"
        src.write_text("some content\n")
        result = classify_file(src, StubClassifierModel())
        # Unknown extension won't be in EXTENSION_TO_LANGUAGE
        assert result is not None
        assert result.language is None


class TestClassifyRepository:
    def test_scans_source_files(self, tmp_path: Path):
        (tmp_path / "app.py").write_text("import os\n")
        (tmp_path / "lib.py").write_text("x = 1\n")

        result = classify_repository(tmp_path, StubClassifierModel())

        assert result.files_scanned == 2
        assert len(result.file_classifications) == 2
        assert result.model_id == "stub-model-v1"

    def test_skips_excluded_dirs(self, tmp_path: Path):
        (tmp_path / "src").mkdir()
        (tmp_path / "src" / "app.py").write_text("import os\n")
        (tmp_path / "node_modules").mkdir()
        (tmp_path / "node_modules" / "lib.js").write_text("var x = 1;\n")

        result = classify_repository(tmp_path, StubClassifierModel())

        assert result.files_scanned == 1
        paths = [fc.file_path for fc in result.file_classifications]
        assert any("app.py" in p for p in paths)
        assert not any("node_modules" in p for p in paths)

    def test_empty_repository(self, tmp_path: Path):
        result = classify_repository(tmp_path, StubClassifierModel())
        assert result.files_scanned == 0
        assert result.file_classifications == ()

    def test_nonexistent_path(self, tmp_path: Path):
        result = classify_repository(tmp_path / "nonexistent", StubClassifierModel())
        assert result.files_scanned == 0
