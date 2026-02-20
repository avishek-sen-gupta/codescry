"""Group integration signals by their enclosing AST context."""

from collections.abc import Callable
from dataclasses import dataclass
from functools import reduce
from itertools import groupby
from operator import attrgetter

from repo_surveyor.detection.integration_detector import IntegrationSignal
from repo_surveyor.integration_patterns import Language
from repo_surveyor.integration_concretiser.ast_walker import extract_ast_context
from repo_surveyor.integration_concretiser.types import ASTContext


@dataclass(frozen=True)
class SignalGroup:
    """A group of signals sharing the same enclosing AST node.

    Attributes:
        ast_context: The shared enclosing AST node context.
        signals: The integration signals in this group.
        file_path: Path to the source file.
    """

    ast_context: ASTContext
    signals: tuple[IntegrationSignal, ...]
    file_path: str


def _read_file_bytes(file_path: str) -> bytes:
    """Read file content as bytes."""
    with open(file_path, "rb") as f:
        return f.read()


@dataclass(frozen=True)
class _SignalWithContext:
    """A signal paired with its resolved AST context and grouping key."""

    signal: IntegrationSignal
    ast_context: ASTContext
    group_key: tuple[str, int, int]


def _resolve_signal(
    signal: IntegrationSignal,
    file_content: bytes,
    language: Language,
) -> _SignalWithContext:
    """Resolve a single signal to its AST context."""
    ctx = extract_ast_context(file_content, language, signal.match.line_number)
    return _SignalWithContext(
        signal=signal,
        ast_context=ctx,
        group_key=(signal.match.file_path, ctx.start_line, ctx.end_line),
    )


def _resolve_file_signals(
    file_path: str,
    signals: list[IntegrationSignal],
    file_reader: Callable[[str], bytes],
) -> tuple[_SignalWithContext, ...]:
    """Resolve all signals in a single file to their AST contexts."""
    first_language = signals[0].match.language
    if first_language is None:
        return ()

    file_content = file_reader(file_path)
    return tuple(
        _resolve_signal(signal, file_content, first_language) for signal in signals
    )


def _group_by_file(
    signals: list[IntegrationSignal],
) -> dict[str, list[IntegrationSignal]]:
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
    signals: list[IntegrationSignal],
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
    by_file = _group_by_file(signals)

    all_resolved = reduce(
        lambda acc, item: acc + _resolve_file_signals(item[0], item[1], file_reader),
        by_file.items(),
        (),
    )

    return _collect_into_groups(all_resolved)
