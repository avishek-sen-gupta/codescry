"""Tests for shared LLM concretiser utilities."""

import pytest

from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_concretiser.llm_shared import (
    MAX_FILE_CHARS,
    SYSTEM_PROMPT,
    build_classification_prompt,
    parse_classification_response,
    read_file_bytes,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language
from repo_surveyor.training.types import TrainingLabel

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_signal(
    file_path: str,
    line_number: int,
    line_content: str,
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content=line_content,
            language=Language.JAVA,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=EntityType.FILE_CONTENT,
        source="test",
    )


# ---------------------------------------------------------------------------
# Tests: SYSTEM_PROMPT
# ---------------------------------------------------------------------------


class TestSystemPrompt:
    def test_contains_valid_labels(self):
        assert "DEFINITE_INWARD" in SYSTEM_PROMPT
        assert "DEFINITE_OUTWARD" in SYSTEM_PROMPT
        assert "NOT_DEFINITE" in SYSTEM_PROMPT

    def test_requests_json_format(self):
        assert "JSON" in SYSTEM_PROMPT


# ---------------------------------------------------------------------------
# Tests: build_classification_prompt
# ---------------------------------------------------------------------------


class TestBuildClassificationPrompt:
    def test_includes_file_path(self):
        signal = _make_signal("Foo.java", 10, "conn.open()")
        prompt = build_classification_prompt("Foo.java", "some code", [signal], False)
        assert "Foo.java" in prompt

    def test_includes_signal_lines(self):
        signal = _make_signal("Foo.java", 10, "conn.open()")
        prompt = build_classification_prompt("Foo.java", "some code", [signal], False)
        assert "Line 10" in prompt
        assert "conn.open()" in prompt

    def test_includes_file_content(self):
        signal = _make_signal("Foo.java", 1, "import foo;")
        prompt = build_classification_prompt(
            "Foo.java", "import foo;\nclass Bar {}", [signal], False
        )
        assert "import foo;" in prompt
        assert "class Bar {}" in prompt

    def test_truncation_note_when_truncated(self):
        signal = _make_signal("Foo.java", 1, "import foo;")
        prompt = build_classification_prompt("Foo.java", "code", [signal], True)
        assert "truncated" in prompt.lower()
        assert str(MAX_FILE_CHARS) in prompt

    def test_no_truncation_note_when_not_truncated(self):
        signal = _make_signal("Foo.java", 1, "import foo;")
        prompt = build_classification_prompt("Foo.java", "code", [signal], False)
        assert "truncated" not in prompt.lower()

    def test_deduplicates_signals_on_same_line(self):
        sig1 = _make_signal("Foo.java", 10, "conn.open()", IntegrationType.HTTP_REST)
        sig2 = _make_signal("Foo.java", 10, "conn.open()", IntegrationType.HTTP_REST)
        prompt = build_classification_prompt(
            "Foo.java", "some code", [sig1, sig2], False
        )
        assert prompt.count("Line 10") == 1

    def test_sorts_signals_by_line_number(self):
        sig_a = _make_signal("Foo.java", 20, "line 20")
        sig_b = _make_signal("Foo.java", 5, "line 5")
        prompt = build_classification_prompt(
            "Foo.java", "some code", [sig_a, sig_b], False
        )
        idx_5 = prompt.index("Line 5")
        idx_20 = prompt.index("Line 20")
        assert idx_5 < idx_20


# ---------------------------------------------------------------------------
# Tests: parse_classification_response
# ---------------------------------------------------------------------------


class TestParseClassificationResponse:
    def test_parses_valid_response(self):
        data = {
            "integrations": [
                {
                    "line": 10,
                    "label": "DEFINITE_OUTWARD",
                    "confidence": 0.9,
                    "reason": "HTTP client call",
                }
            ]
        }
        result = parse_classification_response(data)
        assert 10 in result
        label, conf, reason = result[10]
        assert label == TrainingLabel.DEFINITE_OUTWARD
        assert conf == pytest.approx(0.9)
        assert reason == "HTTP client call"

    def test_parses_multiple_entries(self):
        data = {
            "integrations": [
                {
                    "line": 5,
                    "label": "DEFINITE_INWARD",
                    "confidence": 0.8,
                    "reason": "REST endpoint",
                },
                {
                    "line": 15,
                    "label": "NOT_DEFINITE",
                    "confidence": 0.3,
                    "reason": "Just a string",
                },
            ]
        }
        result = parse_classification_response(data)
        assert len(result) == 2
        assert result[5][0] == TrainingLabel.DEFINITE_INWARD
        assert result[15][0] == TrainingLabel.NOT_DEFINITE

    def test_unknown_label_defaults_to_not_definite(self):
        data = {
            "integrations": [
                {
                    "line": 1,
                    "label": "MAYBE_SOMETHING",
                    "confidence": 0.5,
                    "reason": "unknown",
                }
            ]
        }
        result = parse_classification_response(data)
        assert result[1][0] == TrainingLabel.NOT_DEFINITE

    def test_missing_integrations_key_returns_empty(self):
        result = parse_classification_response({})
        assert result == {}

    def test_none_line_skipped(self):
        data = {
            "integrations": [
                {
                    "line": None,
                    "label": "DEFINITE_INWARD",
                    "confidence": 0.9,
                    "reason": "some reason",
                }
            ]
        }
        result = parse_classification_response(data)
        assert result == {}

    def test_defaults_confidence_to_half(self):
        data = {"integrations": [{"line": 10, "label": "DEFINITE_OUTWARD"}]}
        result = parse_classification_response(data)
        assert result[10][1] == pytest.approx(0.5)

    def test_defaults_reason_to_empty_string(self):
        data = {"integrations": [{"line": 10, "label": "DEFINITE_OUTWARD"}]}
        result = parse_classification_response(data)
        assert result[10][2] == ""


# ---------------------------------------------------------------------------
# Tests: read_file_bytes
# ---------------------------------------------------------------------------


class TestReadFileBytes:
    def test_reads_file(self, tmp_path):
        f = tmp_path / "sample.txt"
        f.write_bytes(b"hello world")
        assert read_file_bytes(str(f)) == b"hello world"

    def test_raises_on_missing_file(self):
        with pytest.raises(OSError):
            read_file_bytes("/no/such/file/exists.txt")
