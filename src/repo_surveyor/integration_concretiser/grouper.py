"""Group integration signals by their enclosing AST context."""

import logging
import time
from collections.abc import Callable
from dataclasses import dataclass
from functools import reduce
from itertools import groupby
from operator import attrgetter

from repo_surveyor.integration_patterns import Language
from repo_surveyor.integration_concretiser.ast_walker import batch_extract_ast_contexts
from repo_surveyor.integration_concretiser.types import ASTContext, SignalLike

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class SignalGroup:
    """A group of signals sharing the same enclosing AST node.

    Attributes:
        ast_context: The shared enclosing AST node context.
        signals: The integration signals in this group.
        file_path: Path to the source file.
    """

    ast_context: ASTContext
    signals: tuple[SignalLike, ...]
    file_path: str


def _read_file_bytes(file_path: str) -> bytes:
    """Read file content as bytes."""
    with open(file_path, "rb") as f:
        return f.read()


@dataclass(frozen=True)
class _SignalWithContext:
    """A signal paired with its resolved AST context and grouping key."""

    signal: SignalLike
    ast_context: ASTContext
    group_key: tuple[str, int, int]


def _resolve_file_signals(
    file_path: str,
    signals: list[SignalLike],
    file_reader: Callable[[str], bytes],
) -> tuple[_SignalWithContext, ...]:
    """Resolve all signals in a single file to their AST contexts.

    Parses the file once and extracts AST contexts for all signal lines
    in a single traversal via batch_extract_ast_contexts.
    """
    first_language = signals[0].match.language
    if first_language is None:
        return ()

    file_content = file_reader(file_path)
    line_numbers = frozenset(sig.match.line_number for sig in signals)
    contexts_by_line = batch_extract_ast_contexts(
        file_content, first_language, line_numbers
    )
    return tuple(
        _SignalWithContext(
            signal=sig,
            ast_context=contexts_by_line[sig.match.line_number],
            group_key=(
                sig.match.file_path,
                contexts_by_line[sig.match.line_number].start_line,
                contexts_by_line[sig.match.line_number].end_line,
            ),
        )
        for sig in signals
    )


def _group_by_file(
    signals: list[SignalLike],
) -> dict[str, list[SignalLike]]:
    """Group signals by file path."""
    sorted_signals = sorted(signals, key=attrgetter("match.file_path"))
    return {
        file_path: list(group)
        for file_path, group in groupby(
            sorted_signals, key=attrgetter("match.file_path")
        )
    }


def _collect_into_groups(
    resolved: tuple[_SignalWithContext, ...],
) -> list[SignalGroup]:
    """Collect resolved signals into SignalGroups by their group key."""
    sorted_resolved = sorted(resolved, key=attrgetter("group_key"))
    return [
        SignalGroup(
            ast_context=group_items[0].ast_context,
            signals=tuple(item.signal for item in group_items),
            file_path=key[0],
        )
        for key, group_iter in groupby(sorted_resolved, key=attrgetter("group_key"))
        for group_items in [list(group_iter)]
    ]


def group_signals_by_ast_context(
    signals: list[SignalLike],
    file_reader: Callable[[str], bytes] = _read_file_bytes,
) -> list[SignalGroup]:
    """Group signals by their enclosing AST node.

    For each unique file, parses once and extracts AST contexts for all
    signals in that file. Signals sharing the same enclosing node
    (same file, start_line, end_line) are grouped together.

    Args:
        signals: Integration signals to group.
        file_reader: Callable that reads a file path and returns bytes.
            Defaults to reading from disk. Inject for testing.

    Returns:
        List of SignalGroups, one per unique enclosing AST node.
    """
    t0 = time.monotonic()
    by_file = _group_by_file(signals)
    total_files = len(by_file)
    logger.info(
        "AST grouping: resolving %d signals across %d unique files",
        len(signals),
        total_files,
    )

    all_resolved: tuple[_SignalWithContext, ...] = ()
    for file_idx, (file_path, file_signals) in enumerate(by_file.items(), 1):
        resolved = _resolve_file_signals(file_path, file_signals, file_reader)
        all_resolved = all_resolved + resolved
        if file_idx % 50 == 0 or file_idx == total_files:
            logger.info(
                "AST grouping: processed %d/%d files (%d signals resolved so far)",
                file_idx,
                total_files,
                len(all_resolved),
            )

    groups = _collect_into_groups(all_resolved)
    elapsed = time.monotonic() - t0
    logger.info(
        "AST grouping complete: %d groups from %d signals in %d files (%.2fs)",
        len(groups),
        len(all_resolved),
        total_files,
        elapsed,
    )
    return groups
