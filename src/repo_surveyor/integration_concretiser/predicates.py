"""Pure predicate functions for evidence-based signal verification.

Each predicate is a pure function ``(PredicateContext, str) -> bool`` where
the ``str`` argument is the pattern_arg from the checklist entry (empty
for non-parametric predicates).

Predicates are organised into three categories:
- **Structural (AST):** inspect ancestor node types or enclosing node metadata
- **File/path:** inspect the file path, directory, or filename
- **Textual:** regex match against line content, enclosing node text, or siblings
"""

import re
from collections.abc import Callable

from repo_surveyor.detection.syntax_zone import _NodeTypes
from repo_surveyor.integration_concretiser.evidence_predicates import PredicateName
from repo_surveyor.integration_concretiser.predicate_context_builder import (
    PredicateContext,
)

# ---------------------------------------------------------------------------
# Structural (AST) predicates
# ---------------------------------------------------------------------------

_STRING_NODE_TYPES = _NodeTypes.STRING


def in_string_context(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if any ancestor node type is a string literal.

    Catches inline strings that the syntax-zone line-level filter misses
    (e.g. a string on a line that also has code).
    """
    return any(t in _STRING_NODE_TYPES for t in ctx.ancestor_node_types)


_CONSTANT_NODE_TYPES = frozenset(
    {
        "const_item",
        "static_item",
        "const_declaration",
        "lexical_declaration",
    }
)

_CONSTANT_TEXT_PATTERN = re.compile(
    r"""(?x)
    \bstatic\s+final\b       # Java static final
    | \bconst\b               # JS/TS/Go/Rust const
    | \bval\b                 # Kotlin val (effectively constant)
    | ^[A-Z][A-Z0-9_]*\s*=   # ALL_CAPS assignment (Python, Ruby)
    """,
    re.MULTILINE,
)


def in_constant_decl(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if the signal is inside a constant or static-final declaration."""
    if ctx.enclosing_node_type in _CONSTANT_NODE_TYPES:
        return True
    return bool(_CONSTANT_TEXT_PATTERN.search(ctx.enclosing_node_text))


_ASSERT_PATTERN = re.compile(
    r"""(?x)
    \bassert\b               # Python/Java assert
    | \bassertEquals\b        # JUnit
    | \bassertThat\b          # Hamcrest/AssertJ
    | \bassertTrue\b
    | \bassertFalse\b
    | \bassertNotNull\b
    | \bassertNull\b
    | \bassertRaises\b        # Python unittest
    | \bexpect\b              # Jest/Mocha/RSpec
    | \.should\b              # Chai/RSpec should
    | \bverify\b              # Mockito verify
    | \bcheck!                # Rust check macro
    | \bassert!               # Rust assert macro
    | \bassert_eq!            # Rust assert_eq macro
    """,
    re.IGNORECASE,
)


def in_assertion(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if the enclosing node contains assert/expect/should patterns."""
    return bool(_ASSERT_PATTERN.search(ctx.enclosing_node_text))


# ---------------------------------------------------------------------------
# File/path predicates
# ---------------------------------------------------------------------------

_TEST_DIR_PATTERN = re.compile(r"""(?x)
    /tests?/
    | /spec/
    | /__tests__/
    | /test_helpers?/
    | /fixtures?/
    """)

_TEST_FILE_PATTERN = re.compile(r"""(?x)
    (?:^|/)test_[^/]+\.py$           # Python test_*.py
    | (?:^|/)[^/]+_test\.py$          # Python *_test.py
    | (?:^|/)[^/]+Test\.java$         # Java *Test.java
    | (?:^|/)[^/]+_test\.go$          # Go *_test.go
    | (?:^|/)[^/]+\.spec\.[jt]sx?$    # JS/TS *.spec.{js,ts,jsx,tsx}
    | (?:^|/)[^/]+\.test\.[jt]sx?$    # JS/TS *.test.{js,ts,tsx}
    | (?:^|/)[^/]+_spec\.rb$          # Ruby *_spec.rb
    | (?:^|/)[^/]+Test\.kt$           # Kotlin *Test.kt
    | (?:^|/)[^/]+Test\.scala$        # Scala *Test.scala
    | (?:^|/)conftest\.py$            # pytest conftest
    """)


def in_test_file(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if the file path matches common test file/directory conventions."""
    fp = ctx.file_path
    return bool(_TEST_DIR_PATTERN.search(fp) or _TEST_FILE_PATTERN.search(fp))


_VENDOR_DIR_PATTERN = re.compile(r"""(?x)
    (?:^|/)vendor/
    | (?:^|/)third[_-]?party/
    | (?:^|/)node_modules/
    | /\.gradle/
    | (?:^|/)build/generated/
    | (?:^|/)target/generated/
    """)


def in_vendor_dir(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if the file is under a vendor or third-party directory."""
    return bool(_VENDOR_DIR_PATTERN.search(ctx.file_path))


_CONFIG_DIR_PATTERN = re.compile(r"""(?x)
    /config/
    | /configs?/
    | /settings?/
    | /\.config/
    | /properties/
    """)

_CONFIG_FILE_PATTERN = re.compile(r"""(?x)
    (?:^|/)[^/]*\.(?:ya?ml|toml|ini|cfg|conf|properties|env)$
    | (?:^|/)\.env(?:\.[^/]*)?$
    """)


def in_config_dir(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if the file is under a config/settings directory or is a config file."""
    fp = ctx.file_path
    return bool(_CONFIG_DIR_PATTERN.search(fp) or _CONFIG_FILE_PATTERN.search(fp))


_GENERATED_MARKERS = re.compile(r"""(?xi)
    /generated/
    | \.generated\.
    | \.g\.dart$
    | _pb2\.py$
    | \.pb\.go$
    | /proto/.*\.java$
    """)

_GENERATED_COMMENT_PATTERN = re.compile(
    r"(?i)auto[\s-]?generated|do\s+not\s+edit|generated\s+by",
)

_GENERATED_HEADER_LINE_COUNT = 5


def in_generated_file(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if the file appears to be auto-generated (path or header comment)."""
    if _GENERATED_MARKERS.search(ctx.file_path):
        return True
    header = "\n".join(ctx.source_lines[:_GENERATED_HEADER_LINE_COUNT])
    return bool(_GENERATED_COMMENT_PATTERN.search(header))


def path_matches(ctx: PredicateContext, pattern_arg: str) -> bool:
    """True if the file path matches the given regex pattern_arg."""
    if not pattern_arg:
        return False
    return bool(re.search(pattern_arg, ctx.file_path))


# ---------------------------------------------------------------------------
# Textual predicates
# ---------------------------------------------------------------------------

_LOG_PATTERN = re.compile(
    r"""(?x)
    \blogger\.\w+\(             # Java/Python logger.info(...) etc.
    | \blog\.\w+\(               # Go log.Println(...) etc.
    | \bconsole\.\w+\(           # JS/TS console.log(...) etc.
    | \blogging\.\w+\(           # Python logging.info(...)
    | \bLog\.\w+\(               # Android Log.d(...) etc.
    | \bslog\.\w+\(              # Go slog structured logging
    | \blog::\w+!\(              # Rust log::info!(...) etc.
    | \btracing::\w+!\(          # Rust tracing::info!(...) etc.
    | \bprintln!\(               # Rust println!(...)
    | \bprint\(                  # Python print(...)
    | \bfmt\.Print              # Go fmt.Println/Printf
    | \bSystem\.out\.print       # Java System.out.println
    | \bSystem\.err\.print       # Java System.err.println
    """,
    re.IGNORECASE,
)


def in_log_statement(ctx: PredicateContext, _pattern_arg: str) -> bool:
    """True if the enclosing node contains common logging calls."""
    return bool(_LOG_PATTERN.search(ctx.enclosing_node_text))


def enclosing_function_calls(ctx: PredicateContext, pattern_arg: str) -> bool:
    """True if the enclosing node text matches the given regex pattern_arg."""
    if not pattern_arg:
        return False
    return bool(re.search(pattern_arg, ctx.enclosing_node_text))


_SIBLING_WINDOW = 5


def sibling_line_matches(ctx: PredicateContext, pattern_arg: str) -> bool:
    """True if any line within +/-5 of the signal matches pattern_arg regex."""
    if not pattern_arg:
        return False
    line_idx = ctx.line_number - 1  # 0-indexed
    start = max(0, line_idx - _SIBLING_WINDOW)
    end = min(len(ctx.source_lines), line_idx + _SIBLING_WINDOW + 1)
    return any(re.search(pattern_arg, ctx.source_lines[i]) for i in range(start, end))


def line_matches(ctx: PredicateContext, pattern_arg: str) -> bool:
    """True if the signal line itself matches the given regex pattern_arg."""
    if not pattern_arg:
        return False
    return bool(re.search(pattern_arg, ctx.line_content))


# ---------------------------------------------------------------------------
# Dispatch table
# ---------------------------------------------------------------------------

PREDICATE_DISPATCH: dict[PredicateName, Callable[[PredicateContext, str], bool]] = {
    PredicateName.IN_STRING_CONTEXT: in_string_context,
    PredicateName.IN_CONSTANT_DECL: in_constant_decl,
    PredicateName.IN_ASSERTION: in_assertion,
    PredicateName.IN_TEST_FILE: in_test_file,
    PredicateName.IN_VENDOR_DIR: in_vendor_dir,
    PredicateName.IN_CONFIG_DIR: in_config_dir,
    PredicateName.IN_GENERATED_FILE: in_generated_file,
    PredicateName.PATH_MATCHES: path_matches,
    PredicateName.IN_LOG_STATEMENT: in_log_statement,
    PredicateName.ENCLOSING_FUNCTION_CALLS: enclosing_function_calls,
    PredicateName.SIBLING_LINE_MATCHES: sibling_line_matches,
    PredicateName.LINE_MATCHES: line_matches,
}
