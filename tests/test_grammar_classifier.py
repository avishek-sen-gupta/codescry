"""Tests for the grammar classifier prompt, parser, and LLM integration."""

import json

import pytest

from repo_surveyor.cfg_constructor import (
    ControlFlowRole,
    LanguageCFGSpec,
    parse_classification_response,
)
from repo_surveyor.cfg_constructor.grammar_classifier import (
    SYSTEM_PROMPT,
    build_user_prompt,
    extract_node_types,
)
from repo_surveyor.integration_patterns.types import Language


class TestParseClassificationResponse:
    def test_basic(self):
        text = (
            "if_statement|BRANCH\n"
            "while_statement|LOOP\n"
            "block|SEQUENCE\n"
            "return_statement|RETURN\n"
        )
        result = parse_classification_response(text)
        assert result == {
            "if_statement": ControlFlowRole.BRANCH,
            "while_statement": ControlFlowRole.LOOP,
            "block": ControlFlowRole.SEQUENCE,
            "return_statement": ControlFlowRole.RETURN,
        }

    def test_skips_malformed_lines(self):
        text = (
            "if_statement|BRANCH\n"
            "this line has no pipe\n"
            "too|many|pipes\n"
            "unknown_node|NOT_A_ROLE\n"
            "while_statement|LOOP\n"
        )
        result = parse_classification_response(text)
        assert result == {
            "if_statement": ControlFlowRole.BRANCH,
            "while_statement": ControlFlowRole.LOOP,
        }

    def test_case_insensitive(self):
        text = "if_statement|branch\nwhile_statement|LOOP\nfor_statement|Loop\n"
        result = parse_classification_response(text)
        assert result == {
            "if_statement": ControlFlowRole.BRANCH,
            "while_statement": ControlFlowRole.LOOP,
            "for_statement": ControlFlowRole.LOOP,
        }

    def test_strips_whitespace(self):
        text = "  if_statement  |  BRANCH  \n"
        result = parse_classification_response(text)
        assert result == {"if_statement": ControlFlowRole.BRANCH}

    def test_empty_input(self):
        assert parse_classification_response("") == {}

    def test_all_roles_parseable(self):
        lines = [f"node_{role.name.lower()}|{role.name}" for role in ControlFlowRole]
        text = "\n".join(lines)
        result = parse_classification_response(text)
        assert len(result) == len(ControlFlowRole)


class TestSystemPrompt:
    def test_contains_all_roles(self):
        for role in ControlFlowRole:
            assert (
                role.name in SYSTEM_PROMPT
            ), f"Missing role {role.name} in SYSTEM_PROMPT"

    def test_contains_output_format_instruction(self):
        assert "NODE_TYPE|ROLE" in SYSTEM_PROMPT


class TestExtractNodeTypes:
    _MINIMAL_GRAMMAR = """\
module.exports = grammar({
  name: 'test_lang',
  rules: {
    program: $ => repeat($.statement),
    if_statement: $ => seq('if', '(', $.expression, ')', $.block),
    block: ($) => seq('{', repeat($.statement), '}'),
    expression: $ => choice($.identifier, $.number),
  }
});
"""

    def test_extracts_rule_names(self):
        result = extract_node_types(self._MINIMAL_GRAMMAR)
        assert "program" in result
        assert "if_statement" in result
        assert "block" in result
        assert "expression" in result

    def test_returns_sorted(self):
        result = extract_node_types(self._MINIMAL_GRAMMAR)
        assert result == sorted(result)

    def test_raises_on_invalid_grammar(self):
        with pytest.raises(RuntimeError):
            extract_node_types("this is not valid javascript;!@#")


class TestBuildUserPrompt:
    def test_lists_node_types(self):
        prompt = build_user_prompt(["block", "if_statement", "while_statement"])
        assert "block" in prompt
        assert "if_statement" in prompt
        assert "while_statement" in prompt

    def test_empty_list(self):
        prompt = build_user_prompt([])
        assert "grammar" in prompt.lower()


_JAVA_GRAMMAR_URL = (
    "https://raw.githubusercontent.com/tree-sitter/tree-sitter-java/master/grammar.js"
)

_EXPECTED_JAVA_CLASSIFICATIONS = {
    "if_statement": ControlFlowRole.BRANCH,
    "while_statement": ControlFlowRole.LOOP,
    "for_statement": ControlFlowRole.LOOP,
    "return_statement": ControlFlowRole.RETURN,
    "break_statement": ControlFlowRole.BREAK,
    "try_statement": ControlFlowRole.TRY,
    "block": ControlFlowRole.SEQUENCE,
    "throw_statement": ControlFlowRole.THROW,
}


def _ollama_available() -> bool:
    """Check whether Ollama is importable and reachable."""
    try:
        import ollama  # type: ignore[import-not-found]

        ollama.Client().list()
        return True
    except Exception:
        return False


@pytest.mark.ollama
@pytest.mark.skipif(not _ollama_available(), reason="Ollama not available")
class TestClassifyJavaGrammarWithLLM:
    def test_classify_java_grammar(self):
        import requests

        from repo_surveyor.ml_classifier.qwen_model import QwenClassifierModel

        response = requests.get(_JAVA_GRAMMAR_URL, timeout=30)
        response.raise_for_status()
        grammar_js = response.text

        node_types = extract_node_types(grammar_js)
        model = QwenClassifierModel(model="qwen2.5-coder:7b-instruct")
        user_prompt = build_user_prompt(node_types)
        completion = model.classify(SYSTEM_PROMPT, user_prompt)

        classifications = parse_classification_response(completion.text)
        spec = LanguageCFGSpec(language=Language.JAVA, node_specs=classifications)

        output = {
            node_type: role.name for node_type, role in sorted(classifications.items())
        }
        print(f"\n{json.dumps(output, indent=2)}\n")

        for node_type, expected_role in _EXPECTED_JAVA_CLASSIFICATIONS.items():
            actual = spec.role_for(node_type)
            assert (
                actual == expected_role
            ), f"{node_type}: expected {expected_role.name}, got {actual.name}"
