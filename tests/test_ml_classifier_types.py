"""Tests for ml_classifier types."""

from repo_surveyor.ml_classifier.types import (
    ClassifiedLine,
    CompletionResult,
    FileClassification,
    MLIntegrationType,
    RepositoryClassification,
)


class TestMLIntegrationType:
    def test_has_21_members(self):
        assert len(MLIntegrationType) == 21

    def test_none_is_last(self):
        values = list(MLIntegrationType)
        assert values[-1] == MLIntegrationType.NONE

    def test_value_is_lowercase(self):
        for member in MLIntegrationType:
            assert member.value == member.value.lower()

    def test_key_categories_exist(self):
        expected = {
            "HTTP_CLIENT",
            "HTTP_SERVER",
            "REST_ENDPOINT",
            "GRAPHQL",
            "GRPC",
            "WEBSOCKET",
            "SOAP",
            "TCP_SOCKET",
            "UDP_SOCKET",
            "DATABASE_QUERY",
            "DATABASE_CONNECTION",
            "ORM",
            "CACHE",
            "MESSAGE_PUBLISH",
            "MESSAGE_SUBSCRIBE",
            "STREAM_PRODUCER",
            "STREAM_CONSUMER",
            "FILE_IO",
            "PROCESS_EXEC",
            "FFI",
            "NONE",
        }
        actual = {member.name for member in MLIntegrationType}
        assert actual == expected


class TestCompletionResult:
    def test_is_frozen(self):
        result = CompletionResult(text="hello", prompt_tokens=10, completion_tokens=5)
        assert result.text == "hello"
        assert result.prompt_tokens == 10
        assert result.completion_tokens == 5

    def test_immutable(self):
        result = CompletionResult(text="hello", prompt_tokens=10, completion_tokens=5)
        try:
            result.text = "other"  # type: ignore[misc]
            assert False, "Should have raised"
        except AttributeError:
            pass


class TestClassifiedLine:
    def test_fields(self):
        line = ClassifiedLine(
            line_number=42,
            line_content="import requests",
            integration_type=MLIntegrationType.HTTP_CLIENT,
            raw_model_label="http_client",
        )
        assert line.line_number == 42
        assert line.line_content == "import requests"
        assert line.integration_type == MLIntegrationType.HTTP_CLIENT
        assert line.raw_model_label == "http_client"


class TestFileClassification:
    def test_fields(self):
        fc = FileClassification(
            file_path="/src/app.py",
            language="Python",
            classified_lines=(),
            lines_submitted=100,
            lines_skipped=20,
        )
        assert fc.file_path == "/src/app.py"
        assert fc.language == "Python"
        assert fc.classified_lines == ()
        assert fc.lines_submitted == 100
        assert fc.lines_skipped == 20


class TestRepositoryClassification:
    def test_fields(self):
        rc = RepositoryClassification(
            file_classifications=(),
            files_scanned=5,
            model_id="test-model",
        )
        assert rc.file_classifications == ()
        assert rc.files_scanned == 5
        assert rc.model_id == "test-model"
