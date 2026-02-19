"""Tests for training data generator (parsing logic only, no API calls)."""

import json

from repo_surveyor.training.generator import (
    _parse_generated_examples,
    _build_user_prompt,
    _make_example_id,
    _format_patterns,
)
from repo_surveyor.training.types import TrainingLabel


class TestMakeExampleId:
    def test_simple_language(self):
        result = _make_example_id("Java", "http_rest", "DEFINITE_INWARD", "Spring", 1)
        assert result == "java__http_rest__definite_inward__spring__001"

    def test_csharp_escaping(self):
        result = _make_example_id("C#", "database", "DEFINITE_OUTWARD", "EF Core", 2)
        assert result == "csharp__database__definite_outward__ef_core__002"

    def test_cpp_escaping(self):
        result = _make_example_id("C++", "socket", "NOT_DEFINITE", "stdlib", 3)
        assert result == "cpp__socket__not_definite__stdlib__003"

    def test_pli_escaping(self):
        result = _make_example_id("PL/I", "file_io", "DEFINITE_OUTWARD", "stdlib", 1)
        assert result == "pli__file_io__definite_outward__stdlib__001"


class TestFormatPatterns:
    def test_formats_numbered_list(self):
        result = _format_patterns(("@GetMapping", r"@PostMapping", r"RestTemplate"))
        assert "1. @GetMapping" in result
        assert "2. @PostMapping" in result
        assert "3. RestTemplate" in result


class TestBuildUserPrompt:
    def test_contains_language_and_type(self):
        prompt = _build_user_prompt(
            "Java",
            "http_rest",
            TrainingLabel.DEFINITE_INWARD,
            ("@GetMapping",),
            5,
        )
        assert "Java" in prompt
        assert "http_rest" in prompt
        assert "DEFINITE_INWARD" in prompt
        assert "@GetMapping" in prompt
        assert "5" in prompt

    def test_not_definite_guidance(self):
        prompt = _build_user_prompt(
            "Python",
            "database",
            TrainingLabel.NOT_DEFINITE,
            ("cursor.execute",),
            3,
        )
        assert "Type declarations" in prompt
        assert "Configuration" in prompt


class TestParseGeneratedExamples:
    def test_parses_valid_json_array(self):
        raw = json.dumps(
            [
                {
                    "code_snippet": '@GetMapping("/users")\npublic List<User> list() {}',
                    "signal_line_index": 0,
                    "signal_line_content": '@GetMapping("/users")',
                    "matched_pattern": "@GetMapping",
                    "ast_node_type": "method_declaration",
                    "framework": "Spring",
                }
            ]
        )
        examples = _parse_generated_examples(
            raw, "Java", "http_rest", "DEFINITE_INWARD"
        )
        assert len(examples) == 1
        assert examples[0].language == "Java"
        assert examples[0].integration_type == "http_rest"
        assert examples[0].label == "DEFINITE_INWARD"
        assert examples[0].framework == "Spring"

    def test_strips_markdown_fencing(self):
        inner = json.dumps(
            [
                {
                    "code_snippet": "code()",
                    "signal_line_index": 0,
                    "signal_line_content": "code()",
                    "matched_pattern": "code",
                    "ast_node_type": "call_expression",
                    "framework": "stdlib",
                }
            ]
        )
        raw = f"```json\n{inner}\n```"
        examples = _parse_generated_examples(
            raw, "Python", "database", "DEFINITE_OUTWARD"
        )
        assert len(examples) == 1

    def test_handles_non_array_response(self):
        examples = _parse_generated_examples(
            '{"not": "an array"}', "Java", "http_rest", "DEFINITE_INWARD"
        )
        assert examples == []

    def test_generates_sequential_ids(self):
        raw = json.dumps(
            [
                {
                    "code_snippet": "a()",
                    "signal_line_index": 0,
                    "signal_line_content": "a()",
                    "matched_pattern": "a",
                    "ast_node_type": "call",
                    "framework": "Spring",
                },
                {
                    "code_snippet": "b()",
                    "signal_line_index": 0,
                    "signal_line_content": "b()",
                    "matched_pattern": "b",
                    "ast_node_type": "call",
                    "framework": "Spring",
                },
            ]
        )
        examples = _parse_generated_examples(
            raw, "Java", "http_rest", "DEFINITE_INWARD"
        )
        assert examples[0].id.endswith("__001")
        assert examples[1].id.endswith("__002")

    def test_missing_framework_defaults_to_unknown(self):
        raw = json.dumps(
            [
                {
                    "code_snippet": "x()",
                    "signal_line_index": 0,
                    "signal_line_content": "x()",
                    "matched_pattern": "x",
                    "ast_node_type": "call",
                }
            ]
        )
        examples = _parse_generated_examples(raw, "Go", "grpc", "DEFINITE_OUTWARD")
        assert examples[0].framework == "unknown"
