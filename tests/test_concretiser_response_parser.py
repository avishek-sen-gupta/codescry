"""Tests for concretiser response parser."""

from repo_surveyor.integration_concretiser.response_parser import parse_response
from repo_surveyor.integration_concretiser.grouper import SignalGroup
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    IntegrationDirection,
)
from repo_surveyor.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language


def _make_signal(
    line_number: int,
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path="test.java",
            line_number=line_number,
            line_content=f"line {line_number}",
            language=Language.JAVA,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=EntityType.FILE_CONTENT,
        source="test",
    )


SAMPLE_CONTEXT = ASTContext(
    node_type="method_declaration",
    node_text="public void handle() { }",
    start_line=5,
    end_line=7,
)


def _make_group(num_signals: int) -> SignalGroup:
    return SignalGroup(
        ast_context=SAMPLE_CONTEXT,
        signals=tuple(_make_signal(i + 1) for i in range(num_signals)),
        file_path="test.java",
    )


class TestParseResponse:
    """Tests for response parsing."""

    def test_parse_definite_inward(self):
        group = _make_group(1)
        response = "0|DEFINITE|INWARD|HTTP handler endpoint"
        results = parse_response(response, group)
        assert len(results) == 1
        assert results[0].is_definite is True
        assert results[0].direction == IntegrationDirection.INWARD
        assert results[0].reasoning == "HTTP handler endpoint"

    def test_parse_definite_outward(self):
        group = _make_group(1)
        response = "0|DEFINITE|OUTWARD|External API call"
        results = parse_response(response, group)
        assert results[0].is_definite is True
        assert results[0].direction == IntegrationDirection.OUTWARD

    def test_parse_not_definite(self):
        group = _make_group(1)
        response = "0|NOT_DEFINITE||Import statement only"
        results = parse_response(response, group)
        assert results[0].is_definite is False
        assert results[0].direction == IntegrationDirection.UNKNOWN
        assert results[0].reasoning == "Import statement only"

    def test_parse_multiple_signals(self):
        group = _make_group(3)
        response = (
            "0|DEFINITE|INWARD|Handler\n"
            "1|NOT_DEFINITE||Just an import\n"
            "2|DEFINITE|OUTWARD|DB query"
        )
        results = parse_response(response, group)
        assert len(results) == 3
        assert results[0].is_definite is True
        assert results[1].is_definite is False
        assert results[2].is_definite is True
        assert results[2].direction == IntegrationDirection.OUTWARD

    def test_missing_signal_defaults_to_not_definite(self):
        group = _make_group(2)
        response = "0|DEFINITE|INWARD|Handler"
        results = parse_response(response, group)
        assert len(results) == 2
        assert results[0].is_definite is True
        assert results[1].is_definite is False
        assert "No valid LLM response" in results[1].reasoning

    def test_malformed_line_skipped(self):
        group = _make_group(1)
        response = "this is garbage\n0|DEFINITE|OUTWARD|Valid line"
        results = parse_response(response, group)
        assert results[0].is_definite is True

    def test_empty_response_defaults_all_to_not_definite(self):
        group = _make_group(2)
        response = ""
        results = parse_response(response, group)
        assert all(not r.is_definite for r in results)

    def test_out_of_range_index_ignored(self):
        group = _make_group(1)
        response = "0|DEFINITE|INWARD|Valid\n5|DEFINITE|OUTWARD|Out of range"
        results = parse_response(response, group)
        assert len(results) == 1
        assert results[0].is_definite is True

    def test_comment_lines_ignored(self):
        group = _make_group(1)
        response = "# This is a comment\n0|DEFINITE|INWARD|Handler"
        results = parse_response(response, group)
        assert results[0].is_definite is True

    def test_preserves_original_signal(self):
        group = _make_group(1)
        response = "0|DEFINITE|INWARD|Handler"
        results = parse_response(response, group)
        assert results[0].original_signal is group.signals[0]

    def test_preserves_ast_context(self):
        group = _make_group(1)
        response = "0|DEFINITE|INWARD|Handler"
        results = parse_response(response, group)
        assert results[0].ast_context is SAMPLE_CONTEXT

    def test_case_insensitive_classification(self):
        group = _make_group(1)
        response = "0|definite|inward|Handler"
        results = parse_response(response, group)
        assert results[0].is_definite is True
        assert results[0].direction == IntegrationDirection.INWARD
