"""Tests for Gemini Flash integration signal concretisation."""

import pytest

from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
)
from repo_surveyor.integration_concretiser.gemini_concretiser import (
    GeminiClassificationClient,
    _MAX_BATCH_PROMPT_CHARS,
    _apply_line_map,
    _concretise_file,
    concretise_with_gemini,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    CompositeIntegrationSignal,
    ConcretisedSignal,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language
from repo_surveyor.training.types import TrainingLabel

# ---------------------------------------------------------------------------
# Fixtures / Helpers
# ---------------------------------------------------------------------------

_PYTHON_SOURCE = b"""\
import requests

class MyService:
    def fetch_data(self, url):
        response = requests.get(url)
        return response.json()

    def handle_request(self):
        pass

def standalone_function():
    db.execute("SELECT * FROM users")
"""

_JAVA_SOURCE = b"""\
package com.example;

import org.springframework.web.bind.annotation.*;

@RestController
public class UserController {

    @GetMapping("/users")
    public List<User> getUsers() {
        return userService.findAll();
    }
}
"""

_FAKE_FILES: dict[str, bytes] = {
    "client.py": _PYTHON_SOURCE,
    "UserController.java": _JAVA_SOURCE,
}


def _fake_reader(path: str) -> bytes:
    return _FAKE_FILES[path]


def _make_signal(
    file_path: str,
    line_number: int,
    line_content: str,
    language: Language = Language.PYTHON,
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
    entity_type: EntityType = EntityType.FILE_CONTENT,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content=line_content,
            language=language,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=entity_type,
        source="test",
    )


class _MockGeminiClient:
    """Mock GeminiClassificationClient that returns pre-configured responses."""

    def __init__(self, responses: list[dict]) -> None:
        self._response_queue = list(responses)
        self.call_log: list[tuple[str, str]] = []

    def classify(self, system_prompt: str, user_prompt: str) -> dict:
        self.call_log.append((system_prompt, user_prompt))
        if not self._response_queue:
            return {}
        return self._response_queue.pop(0)


# ---------------------------------------------------------------------------
# Tests: _concretise_file
# ---------------------------------------------------------------------------


class TestConcretiseFile:
    def test_classifies_signals_from_response(self):
        mock_client = _MockGeminiClient(
            [
                {
                    "integrations": [
                        {
                            "line": 5,
                            "label": "DEFINITE_OUTWARD",
                            "confidence": 0.95,
                            "reason": "HTTP client call",
                        }
                    ]
                }
            ]
        )
        signal = _make_signal("client.py", 5, "requests.get(url)")
        signal_to_ast = {
            ("client.py", 5): ASTContext("call", "requests.get(url)", 5, 5)
        }

        concretised, metadata = _concretise_file(
            "client.py", [signal], signal_to_ast, mock_client, _fake_reader
        )

        assert len(concretised) == 1
        assert concretised[0].label == TrainingLabel.DEFINITE_OUTWARD
        assert metadata[("client.py", 5)]["confidence"] == pytest.approx(0.95)
        assert metadata[("client.py", 5)]["reason"] == "HTTP client call"

    def test_missing_line_defaults_to_not_definite(self):
        mock_client = _MockGeminiClient([{"integrations": []}])
        signal = _make_signal("client.py", 5, "requests.get(url)")
        signal_to_ast = {}

        concretised, metadata = _concretise_file(
            "client.py", [signal], signal_to_ast, mock_client, _fake_reader
        )

        assert concretised[0].label == TrainingLabel.NOT_DEFINITE
        assert metadata[("client.py", 5)]["confidence"] is None

    def test_unreadable_file_returns_not_definite(self):
        def _bad_reader(path: str) -> bytes:
            raise OSError("permission denied")

        mock_client = _MockGeminiClient([])
        signal = _make_signal("missing.py", 1, "some code")

        concretised, metadata = _concretise_file(
            "missing.py", [signal], {}, mock_client, _bad_reader
        )

        assert len(concretised) == 1
        assert concretised[0].label == TrainingLabel.NOT_DEFINITE
        assert metadata == {}

    def test_sends_prompt_to_client(self):
        mock_client = _MockGeminiClient([{"integrations": []}])
        signal = _make_signal("client.py", 5, "requests.get(url)")

        _concretise_file("client.py", [signal], {}, mock_client, _fake_reader)

        assert len(mock_client.call_log) == 1
        system_prompt, user_prompt = mock_client.call_log[0]
        assert "DEFINITE_INWARD" in system_prompt
        assert "client.py" in user_prompt
        assert "Line 5" in user_prompt


# ---------------------------------------------------------------------------
# Tests: concretise_with_gemini (end-to-end with mock)
# ---------------------------------------------------------------------------


class TestConcretiseWithGemini:
    def test_end_to_end_with_mock_client(self, monkeypatch):
        responses = [
            {
                "integrations": [
                    {
                        "line": 5,
                        "label": "DEFINITE_OUTWARD",
                        "confidence": 0.9,
                        "reason": "HTTP client",
                    },
                    {
                        "line": 12,
                        "label": "DEFINITE_OUTWARD",
                        "confidence": 0.85,
                        "reason": "DB query",
                    },
                ]
            }
        ]
        mock_client = _MockGeminiClient(responses)

        monkeypatch.setattr(
            "repo_surveyor.integration_concretiser.gemini_concretiser.GeminiClassificationClient",
            lambda api_key, model: mock_client,
        )

        signals = [
            _make_signal("client.py", 5, "requests.get(url)"),
            _make_signal("client.py", 12, 'db.execute("SELECT")'),
        ]
        detector_result = IntegrationDetectorResult(
            integration_points=signals, files_scanned=1
        )

        result, metadata = concretise_with_gemini(
            detector_result,
            api_key="fake-key",
            file_reader=_fake_reader,
        )

        assert result.signals_submitted == 2
        assert result.signals_definite == 2
        assert result.signals_discarded == 0
        assert result.concretised[0].label == TrainingLabel.DEFINITE_OUTWARD
        assert result.concretised[1].label == TrainingLabel.DEFINITE_OUTWARD

    def test_filters_non_file_content_signals(self, monkeypatch):
        mock_client = _MockGeminiClient([{"integrations": []}])
        monkeypatch.setattr(
            "repo_surveyor.integration_concretiser.gemini_concretiser.GeminiClassificationClient",
            lambda api_key, model: mock_client,
        )

        file_signal = _make_signal("client.py", 5, "requests.get(url)")
        dir_signal = _make_signal(
            "client.py",
            5,
            "requests.get(url)",
            entity_type=EntityType.DIRECTORY,
        )
        detector_result = IntegrationDetectorResult(
            integration_points=[file_signal, dir_signal], files_scanned=1
        )

        result, _ = concretise_with_gemini(
            detector_result,
            api_key="fake-key",
            file_reader=_fake_reader,
        )

        assert result.signals_submitted == 1

    def test_processes_multiple_files_batched(self, monkeypatch):
        """Two small files should be batched into a single API call."""
        # Batched response includes "file" field
        responses = [
            {
                "integrations": [
                    {
                        "file": "UserController.java",
                        "line": 10,
                        "label": "DEFINITE_INWARD",
                        "confidence": 0.85,
                        "reason": "REST endpoint",
                    },
                    {
                        "file": "client.py",
                        "line": 5,
                        "label": "DEFINITE_OUTWARD",
                        "confidence": 0.9,
                        "reason": "HTTP call",
                    },
                ]
            },
        ]
        mock_client = _MockGeminiClient(responses)
        monkeypatch.setattr(
            "repo_surveyor.integration_concretiser.gemini_concretiser.GeminiClassificationClient",
            lambda api_key, model: mock_client,
        )

        signals = [
            _make_signal("client.py", 5, "requests.get(url)"),
            _make_signal(
                "UserController.java",
                10,
                "userService.findAll()",
                language=Language.JAVA,
            ),
        ]
        detector_result = IntegrationDetectorResult(
            integration_points=signals, files_scanned=2
        )

        result, metadata = concretise_with_gemini(
            detector_result,
            api_key="fake-key",
            file_reader=_fake_reader,
        )

        assert result.signals_submitted == 2
        assert result.signals_definite == 2
        labels = [s.label for s in result.concretised]
        assert TrainingLabel.DEFINITE_OUTWARD in labels
        assert TrainingLabel.DEFINITE_INWARD in labels
        # Should have made only 1 API call (batched), not 2
        assert len(mock_client.call_log) == 1

    def test_single_file_batch_uses_single_file_prompt(self, monkeypatch):
        """A batch with only 1 file should use the single-file prompt (no 'file' field needed)."""
        responses = [
            {
                "integrations": [
                    {
                        "line": 5,
                        "label": "DEFINITE_OUTWARD",
                        "confidence": 0.9,
                        "reason": "HTTP call",
                    }
                ]
            },
        ]
        mock_client = _MockGeminiClient(responses)
        monkeypatch.setattr(
            "repo_surveyor.integration_concretiser.gemini_concretiser.GeminiClassificationClient",
            lambda api_key, model: mock_client,
        )

        signals = [_make_signal("client.py", 5, "requests.get(url)")]
        detector_result = IntegrationDetectorResult(
            integration_points=signals, files_scanned=1
        )

        result, _ = concretise_with_gemini(
            detector_result,
            api_key="fake-key",
            file_reader=_fake_reader,
        )

        assert result.signals_submitted == 1
        assert result.signals_definite == 1
        # Single-file prompt should NOT use batched system prompt
        system_prompt_used = mock_client.call_log[0][0]
        assert "MULTIPLE" not in system_prompt_used

    def test_unreadable_file_returns_not_definite(self, monkeypatch):
        """Files that can't be read should get NOT_DEFINITE labels."""
        read_counts: dict[str, int] = {}

        def _partial_reader(path: str) -> bytes:
            read_counts[path] = read_counts.get(path, 0) + 1
            # Allow first read (AST grouper) but fail on second (concretiser)
            if path == "missing.py" and read_counts[path] > 1:
                raise OSError("not found")
            if path == "missing.py":
                return b"some code\n"
            return _FAKE_FILES[path]

        mock_client = _MockGeminiClient(
            [
                {
                    "integrations": [
                        {
                            "file": "client.py",
                            "line": 5,
                            "label": "DEFINITE_OUTWARD",
                            "confidence": 0.9,
                            "reason": "HTTP call",
                        }
                    ]
                }
            ]
        )
        monkeypatch.setattr(
            "repo_surveyor.integration_concretiser.gemini_concretiser.GeminiClassificationClient",
            lambda api_key, model: mock_client,
        )

        signals = [
            _make_signal("client.py", 5, "requests.get(url)"),
            _make_signal("missing.py", 1, "some code"),
        ]
        detector_result = IntegrationDetectorResult(
            integration_points=signals, files_scanned=2
        )

        result, _ = concretise_with_gemini(
            detector_result,
            api_key="fake-key",
            file_reader=_partial_reader,
        )

        assert result.signals_submitted == 2
        labels = {
            s.original_signal.match.file_path: s.label for s in result.concretised
        }
        assert labels["client.py"] == TrainingLabel.DEFINITE_OUTWARD
        assert labels["missing.py"] == TrainingLabel.NOT_DEFINITE

    def test_deduplicates_signals_on_same_line(self, monkeypatch):
        """Duplicate signals on the same line should be merged into one composite."""
        responses = [
            {
                "integrations": [
                    {
                        "line": 5,
                        "label": "DEFINITE_OUTWARD",
                        "confidence": 0.9,
                        "reason": "HTTP client",
                    },
                ]
            }
        ]
        mock_client = _MockGeminiClient(responses)
        monkeypatch.setattr(
            "repo_surveyor.integration_concretiser.gemini_concretiser.GeminiClassificationClient",
            lambda api_key, model: mock_client,
        )

        sig1 = _make_signal(
            "client.py",
            5,
            "requests.get(url)",
            integration_type=IntegrationType.HTTP_REST,
        )
        sig2 = _make_signal(
            "client.py",
            5,
            "requests.get(url)",
            integration_type=IntegrationType.DATABASE,
        )
        detector_result = IntegrationDetectorResult(
            integration_points=[sig1, sig2], files_scanned=1
        )

        result, metadata = concretise_with_gemini(
            detector_result,
            api_key="fake-key",
            file_reader=_fake_reader,
        )

        # Should produce 1 concretised signal (deduped), not 2
        assert result.signals_submitted == 1
        assert result.signals_definite == 1
        # The original_signal should be a CompositeIntegrationSignal
        assert isinstance(
            result.concretised[0].original_signal, CompositeIntegrationSignal
        )
        assert len(result.concretised[0].original_signal.signals) == 2


# ---------------------------------------------------------------------------
# Tests: _apply_line_map
# ---------------------------------------------------------------------------


class TestApplyLineMap:
    def test_applies_labels_from_line_map(self):
        signal = _make_signal("client.py", 5, "requests.get(url)")
        line_map = {5: (TrainingLabel.DEFINITE_OUTWARD, 0.95, "HTTP call")}
        signal_to_ast = {("client.py", 5): ASTContext("call", "requests.get", 5, 5)}

        concretised, metadata = _apply_line_map(
            "client.py", [signal], line_map, signal_to_ast
        )

        assert len(concretised) == 1
        assert concretised[0].label == TrainingLabel.DEFINITE_OUTWARD
        assert metadata[("client.py", 5)]["confidence"] == pytest.approx(0.95)

    def test_missing_line_defaults_to_not_definite(self):
        signal = _make_signal("client.py", 5, "requests.get(url)")

        concretised, metadata = _apply_line_map("client.py", [signal], {}, {})

        assert concretised[0].label == TrainingLabel.NOT_DEFINITE
        assert metadata[("client.py", 5)]["confidence"] is None
