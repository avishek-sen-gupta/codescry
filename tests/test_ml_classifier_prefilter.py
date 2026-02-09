"""Tests for ml_classifier prefilter."""

import pytest

from repo_surveyor.ml_classifier.prefilter import is_trivial_line, prefilter_lines


class TestIsTrivialLine:
    @pytest.mark.parametrize(
        "line",
        [
            "",
            "   ",
            "\t",
            "{",
            "  }",
            "  {}",
            "  {  }",
            "[]",
            "()",
            "//",
            "  //  ",
            "#",
            "  #  ",
            " * ",
            "/*",
            " /** ",
            "*/",
            " */ ",
            "*/",
            '  """  ',
            "  '''  ",
        ],
    )
    def test_trivial_lines_are_detected(self, line):
        assert is_trivial_line(line) is True

    @pytest.mark.parametrize(
        "line",
        [
            "import os",
            "// This is a comment with text",
            "# This is a comment with text",
            "x = 42",
            "def foo():",
            '  println!("hello");',
            "  return result;",
            "  if (x > 0) {",
        ],
    )
    def test_meaningful_lines_are_kept(self, line):
        assert is_trivial_line(line) is False


class TestPrefilterLines:
    def test_empty_input(self):
        kept, skipped = prefilter_lines([])
        assert kept == []
        assert skipped == 0

    def test_all_trivial(self):
        lines = [(1, ""), (2, "  "), (3, "{"), (4, "}")]
        kept, skipped = prefilter_lines(lines)
        assert kept == []
        assert skipped == 4

    def test_all_meaningful(self):
        lines = [(1, "import os"), (2, "x = 1")]
        kept, skipped = prefilter_lines(lines)
        assert kept == [(1, "import os"), (2, "x = 1")]
        assert skipped == 0

    def test_mixed(self):
        lines = [
            (1, "import os"),
            (2, ""),
            (3, "x = 1"),
            (4, "  "),
            (5, "print(x)"),
        ]
        kept, skipped = prefilter_lines(lines)
        assert kept == [(1, "import os"), (3, "x = 1"), (5, "print(x)")]
        assert skipped == 2

    def test_preserves_line_numbers(self):
        lines = [(10, ""), (20, "code"), (30, ""), (40, "more code")]
        kept, skipped = prefilter_lines(lines)
        assert kept == [(20, "code"), (40, "more code")]
        assert skipped == 2
