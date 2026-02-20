"""Tests for ml_classifier prompt construction and chunking."""

from repo_surveyor.ml_classifier.prompt import (
    SYSTEM_PROMPT,
    build_user_prompt,
    chunk_lines,
)
from repo_surveyor.ml_classifier.types import MLIntegrationType


class TestSystemPrompt:
    def test_contains_all_categories(self):
        for member in MLIntegrationType:
            assert member.value in SYSTEM_PROMPT

    def test_contains_format_instruction(self):
        assert "LINE_NUMBER|CATEGORY" in SYSTEM_PROMPT


class TestBuildUserPrompt:
    def test_single_line(self):
        result = build_user_prompt([(1, "import os")])
        assert result == "1|import os"

    def test_multiple_lines(self):
        lines = [(1, "import os"), (3, "x = 1"), (5, "print(x)")]
        result = build_user_prompt(lines)
        assert result == "1|import os\n3|x = 1\n5|print(x)"

    def test_empty_input(self):
        result = build_user_prompt([])
        assert result == ""

    def test_preserves_line_content(self):
        result = build_user_prompt([(42, "  response = requests.get(url)  ")])
        assert result == "42|  response = requests.get(url)  "


class TestChunkLines:
    def test_single_chunk(self):
        lines = [(i, f"line {i}") for i in range(1, 11)]
        chunks = chunk_lines(lines, chunk_size=80)
        assert len(chunks) == 1
        assert chunks[0] == lines

    def test_exact_chunk_boundary(self):
        lines = [(i, f"line {i}") for i in range(1, 5)]
        chunks = chunk_lines(lines, chunk_size=2)
        assert len(chunks) == 2
        assert chunks[0] == [(1, "line 1"), (2, "line 2")]
        assert chunks[1] == [(3, "line 3"), (4, "line 4")]

    def test_partial_last_chunk(self):
        lines = [(i, f"line {i}") for i in range(1, 6)]
        chunks = chunk_lines(lines, chunk_size=2)
        assert len(chunks) == 3
        assert chunks[2] == [(5, "line 5")]

    def test_empty_input(self):
        chunks = chunk_lines([])
        assert chunks == []

    def test_default_chunk_size_is_80(self):
        lines = [(i, f"line {i}") for i in range(1, 161)]
        chunks = chunk_lines(lines)
        assert len(chunks) == 2
        assert len(chunks[0]) == 80
        assert len(chunks[1]) == 80
