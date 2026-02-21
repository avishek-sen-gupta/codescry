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
    SYSTEM_PROMPT_BATCHED,
    build_batched_classification_prompt,
    build_classification_prompt,
    deduplicate_signals,
    map_label_to_validity_direction,
    parse_batched_classification_response,
    parse_classification_response,
    read_file_bytes,
)
from repo_surveyor.integration_concretiser.types import (
    CompositeIntegrationSignal,
    SignalValidity,
)
from repo_surveyor.integration_patterns import (
    Confidence,
    IntegrationType,
    Language,
    SignalDirection,
)

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

    def test_merges_different_types_on_same_line(self):
        sig1 = _make_signal("Foo.java", 38, "conn.open()", IntegrationType.HTTP_REST)
        sig2 = _make_signal("Foo.java", 38, "conn.open()", IntegrationType.DATABASE)
        prompt = build_classification_prompt(
            "Foo.java", "some code", [sig1, sig2], False
        )
        assert prompt.count("Line 38") == 1
        assert "http_rest" in prompt
        assert "database" in prompt

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
        validity, direction, conf, reason = result[10]
        assert validity == SignalValidity.SIGNAL
        assert direction == SignalDirection.OUTWARD
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
        assert result[5][0] == SignalValidity.SIGNAL
        assert result[5][1] == SignalDirection.INWARD
        assert result[15][0] == SignalValidity.NOISE
        assert result[15][1] == SignalDirection.AMBIGUOUS

    def test_unknown_label_defaults_to_noise(self):
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
        assert result[1][0] == SignalValidity.NOISE
        assert result[1][1] == SignalDirection.AMBIGUOUS

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
        assert result[10][2] == pytest.approx(0.5)

    def test_defaults_reason_to_empty_string(self):
        data = {"integrations": [{"line": 10, "label": "DEFINITE_OUTWARD"}]}
        result = parse_classification_response(data)
        assert result[10][3] == ""


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


# ---------------------------------------------------------------------------
# Tests: SYSTEM_PROMPT_BATCHED
# ---------------------------------------------------------------------------


class TestSystemPromptBatched:
    def test_contains_valid_labels(self):
        assert "DEFINITE_INWARD" in SYSTEM_PROMPT_BATCHED
        assert "DEFINITE_OUTWARD" in SYSTEM_PROMPT_BATCHED
        assert "NOT_DEFINITE" in SYSTEM_PROMPT_BATCHED

    def test_requests_json_format(self):
        assert "JSON" in SYSTEM_PROMPT_BATCHED

    def test_mentions_file_field(self):
        assert '"file"' in SYSTEM_PROMPT_BATCHED

    def test_mentions_multiple_files(self):
        assert "MULTIPLE" in SYSTEM_PROMPT_BATCHED


# ---------------------------------------------------------------------------
# Tests: build_batched_classification_prompt
# ---------------------------------------------------------------------------


class TestBuildBatchedClassificationPrompt:
    def test_includes_file_delimiters(self):
        sig_a = _make_signal("Foo.java", 10, "conn.open()")
        sig_b = _make_signal("Bar.java", 5, "db.query()")
        prompt = build_batched_classification_prompt(
            [
                ("Foo.java", "class Foo {}", [sig_a], False),
                ("Bar.java", "class Bar {}", [sig_b], False),
            ]
        )
        assert "===FILE: Foo.java===" in prompt
        assert "===FILE: Bar.java===" in prompt

    def test_includes_signals_from_all_files(self):
        sig_a = _make_signal("Foo.java", 10, "conn.open()")
        sig_b = _make_signal("Bar.java", 5, "db.query()")
        prompt = build_batched_classification_prompt(
            [
                ("Foo.java", "class Foo {}", [sig_a], False),
                ("Bar.java", "class Bar {}", [sig_b], False),
            ]
        )
        assert "Line 10" in prompt
        assert "Line 5" in prompt
        assert "conn.open()" in prompt
        assert "db.query()" in prompt

    def test_includes_file_content(self):
        sig = _make_signal("Foo.java", 1, "x")
        prompt = build_batched_classification_prompt(
            [("Foo.java", "class Foo { int x; }", [sig], False)]
        )
        assert "class Foo { int x; }" in prompt

    def test_ends_with_classify_instruction(self):
        sig = _make_signal("Foo.java", 1, "x")
        prompt = build_batched_classification_prompt(
            [("Foo.java", "code", [sig], False)]
        )
        assert "Classify ALL" in prompt


# ---------------------------------------------------------------------------
# Tests: parse_batched_classification_response
# ---------------------------------------------------------------------------


class TestParseBatchedClassificationResponse:
    def test_parses_multi_file_response(self):
        data = {
            "integrations": [
                {
                    "file": "Foo.java",
                    "line": 10,
                    "label": "DEFINITE_OUTWARD",
                    "confidence": 0.9,
                    "reason": "HTTP call",
                },
                {
                    "file": "Bar.java",
                    "line": 5,
                    "label": "DEFINITE_INWARD",
                    "confidence": 0.85,
                    "reason": "REST endpoint",
                },
            ]
        }
        result = parse_batched_classification_response(data)
        assert "Foo.java" in result
        assert "Bar.java" in result
        assert result["Foo.java"][10][0] == SignalValidity.SIGNAL
        assert result["Foo.java"][10][1] == SignalDirection.OUTWARD
        assert result["Bar.java"][5][0] == SignalValidity.SIGNAL
        assert result["Bar.java"][5][1] == SignalDirection.INWARD

    def test_empty_response(self):
        result = parse_batched_classification_response({})
        assert result == {}

    def test_missing_file_field_skipped(self):
        data = {
            "integrations": [
                {
                    "line": 10,
                    "label": "DEFINITE_OUTWARD",
                    "confidence": 0.9,
                    "reason": "test",
                }
            ]
        }
        result = parse_batched_classification_response(data)
        assert result == {}

    def test_unknown_label_defaults_to_noise(self):
        data = {
            "integrations": [
                {
                    "file": "X.java",
                    "line": 1,
                    "label": "BOGUS",
                    "confidence": 0.5,
                    "reason": "unknown",
                }
            ]
        }
        result = parse_batched_classification_response(data)
        assert result["X.java"][1][0] == SignalValidity.NOISE
        assert result["X.java"][1][1] == SignalDirection.AMBIGUOUS


# ---------------------------------------------------------------------------
# Tests: deduplicate_signals
# ---------------------------------------------------------------------------


class TestDeduplicateSignals:
    def test_single_signal_wrapped_in_composite(self):
        sig = _make_signal("Foo.java", 10, "conn.open()")
        composites = deduplicate_signals([sig])

        assert len(composites) == 1
        assert isinstance(composites[0], CompositeIntegrationSignal)
        assert len(composites[0].signals) == 1
        assert composites[0].signals[0] is sig

    def test_duplicates_merged_into_single_composite(self):
        sig1 = _make_signal("Foo.java", 10, "conn.open()", IntegrationType.HTTP_REST)
        sig2 = _make_signal("Foo.java", 10, "conn.open()", IntegrationType.DATABASE)
        composites = deduplicate_signals([sig1, sig2])

        assert len(composites) == 1
        assert len(composites[0].signals) == 2

    def test_different_lines_remain_separate(self):
        sig1 = _make_signal("Foo.java", 10, "conn.open()")
        sig2 = _make_signal("Foo.java", 20, "db.query()")
        composites = deduplicate_signals([sig1, sig2])

        assert len(composites) == 2

    def test_different_files_remain_separate(self):
        sig1 = _make_signal("Foo.java", 10, "conn.open()")
        sig2 = _make_signal("Bar.java", 10, "conn.open()")
        composites = deduplicate_signals([sig1, sig2])

        assert len(composites) == 2

    def test_composite_delegates_properties(self):
        sig = _make_signal("Foo.java", 10, "conn.open()", IntegrationType.HTTP_REST)
        composites = deduplicate_signals([sig])

        composite = composites[0]
        assert composite.match.file_path == "Foo.java"
        assert composite.match.line_number == 10
        assert composite.integration_type == IntegrationType.HTTP_REST

    def test_empty_list_returns_empty(self):
        composites = deduplicate_signals([])
        assert composites == []

    def test_whitespace_normalisation_in_key(self):
        sig1 = _make_signal("Foo.java", 10, "  conn.open()  ")
        sig2 = _make_signal("Foo.java", 10, "conn.open()")
        composites = deduplicate_signals([sig1, sig2])

        assert len(composites) == 1
        assert len(composites[0].signals) == 2

    def test_composite_to_dict_includes_all_match_details(self):
        sig1 = _make_signal("Foo.java", 10, "conn.open()", IntegrationType.HTTP_REST)
        sig2 = _make_signal("Foo.java", 10, "conn.open()", IntegrationType.DATABASE)
        composites = deduplicate_signals([sig1, sig2])

        d = composites[0].to_dict()
        assert d["file_path"] == "Foo.java"
        assert d["line_number"] == 10
        assert d["line_content"] == "conn.open()"
        assert len(d["match_details"]) == 2
        types = {md["integration_type"] for md in d["match_details"]}
        assert types == {"http_rest", "database"}

    def test_single_signal_to_dict_has_one_match_detail(self):
        sig = _make_signal("Foo.java", 10, "conn.open()")
        d = sig.to_dict()
        assert len(d["match_details"]) == 1
        assert d["match_details"][0]["integration_type"] == "http_rest"
