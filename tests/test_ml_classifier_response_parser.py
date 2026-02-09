"""Tests for ml_classifier response parser."""

from repo_surveyor.ml_classifier.response_parser import parse_classification_response
from repo_surveyor.ml_classifier.types import ClassifiedLine, MLIntegrationType


class TestParseClassificationResponse:
    def test_valid_single_line(self):
        raw = "1|http_client"
        original = {1: "import requests"}
        result = parse_classification_response(raw, original)
        assert len(result) == 1
        assert result[0] == ClassifiedLine(
            line_number=1,
            line_content="import requests",
            integration_type=MLIntegrationType.HTTP_CLIENT,
            raw_model_label="http_client",
        )

    def test_valid_multiple_lines(self):
        raw = "1|http_client\n3|database_query\n5|none"
        original = {1: "import requests", 3: "cursor.execute(sql)", 5: "x = 1"}
        result = parse_classification_response(raw, original)
        assert len(result) == 3
        assert result[0].integration_type == MLIntegrationType.HTTP_CLIENT
        assert result[1].integration_type == MLIntegrationType.DATABASE_QUERY
        assert result[2].integration_type == MLIntegrationType.NONE

    def test_unrecognized_label_falls_back_to_none(self):
        raw = "1|unknown_category"
        original = {1: "some code"}
        result = parse_classification_response(raw, original)
        assert result[0].integration_type == MLIntegrationType.NONE
        assert result[0].raw_model_label == "unknown_category"

    def test_missing_line_number_skipped(self):
        raw = "abc|http_client"
        original = {1: "some code"}
        result = parse_classification_response(raw, original)
        # Line 1 was not responded to, so gets NONE backfill
        assert len(result) == 1
        assert result[0].line_number == 1
        assert result[0].integration_type == MLIntegrationType.NONE

    def test_line_number_not_in_original_skipped(self):
        raw = "99|http_client"
        original = {1: "some code"}
        result = parse_classification_response(raw, original)
        assert len(result) == 1
        assert result[0].line_number == 1
        assert result[0].integration_type == MLIntegrationType.NONE

    def test_malformed_lines_skipped(self):
        raw = "not a valid line\n1|http_client\nalso bad"
        original = {1: "import requests"}
        result = parse_classification_response(raw, original)
        assert len(result) == 1
        assert result[0].integration_type == MLIntegrationType.HTTP_CLIENT

    def test_empty_response(self):
        raw = ""
        original = {1: "code", 2: "more code"}
        result = parse_classification_response(raw, original)
        assert len(result) == 2
        assert all(r.integration_type == MLIntegrationType.NONE for r in result)

    def test_backfill_missing_lines(self):
        raw = "1|http_client"
        original = {1: "import requests", 2: "x = 1", 3: "print(x)"}
        result = parse_classification_response(raw, original)
        assert len(result) == 3
        assert result[0].integration_type == MLIntegrationType.HTTP_CLIENT
        assert result[1].integration_type == MLIntegrationType.NONE
        assert result[1].raw_model_label == ""
        assert result[2].integration_type == MLIntegrationType.NONE

    def test_result_sorted_by_line_number(self):
        raw = "5|http_client\n1|database_query\n3|none"
        original = {1: "a", 3: "b", 5: "c"}
        result = parse_classification_response(raw, original)
        assert [r.line_number for r in result] == [1, 3, 5]

    def test_whitespace_in_label_trimmed(self):
        raw = "1|  http_client  "
        original = {1: "import requests"}
        result = parse_classification_response(raw, original)
        assert result[0].integration_type == MLIntegrationType.HTTP_CLIENT
        assert result[0].raw_model_label == "http_client"

    def test_case_insensitive_label(self):
        raw = "1|HTTP_CLIENT"
        original = {1: "import requests"}
        result = parse_classification_response(raw, original)
        assert result[0].integration_type == MLIntegrationType.HTTP_CLIENT

    def test_pipe_in_code_content(self):
        raw = "1|http_client"
        original = {1: "x = a | b"}
        result = parse_classification_response(raw, original)
        assert result[0].line_content == "x = a | b"
