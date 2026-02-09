"""Pre-filter to skip trivial lines before sending to the model."""

import re

_TRIVIAL_PATTERNS: tuple[re.Pattern[str], ...] = (
    re.compile(r"^\s*$"),
    re.compile(r"^\s*[{}\[\]()][{}\[\]()\s]*$"),
    re.compile(r"^\s*//\s*$"),
    re.compile(r"^\s*#\s*$"),
    re.compile(r"^\s*\*\s*$"),
    re.compile(r"^\s*/\*+\s*$"),
    re.compile(r"^\s*\*+/\s*$"),
    re.compile(r"^\s*\*/\s*$"),
    re.compile(r'^\s*"""\s*$'),
    re.compile(r"^\s*'''\s*$"),
)


def is_trivial_line(line: str) -> bool:
    """Check if a line is trivial and should be skipped.

    Trivial lines include blank lines, brace-only lines,
    and empty comment markers.
    """
    return any(p.match(line) for p in _TRIVIAL_PATTERNS)


def prefilter_lines(
    numbered_lines: list[tuple[int, str]],
) -> tuple[list[tuple[int, str]], int]:
    """Filter out trivial lines from numbered source lines.

    Args:
        numbered_lines: List of (line_number, line_text) tuples.

    Returns:
        Tuple of (kept_lines, skipped_count).
    """
    kept: list[tuple[int, str]] = []
    skipped = 0
    for line_num, line_text in numbered_lines:
        if is_trivial_line(line_text):
            skipped += 1
        else:
            kept.append((line_num, line_text))
    return kept, skipped
