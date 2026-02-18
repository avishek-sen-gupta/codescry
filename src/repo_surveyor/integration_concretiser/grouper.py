"""Group integration signals by their enclosing AST context."""

from collections import defaultdict
from dataclasses import dataclass

from ..integration_detector import IntegrationSignal
from ..integration_patterns import Language
from .ast_walker import extract_ast_context
from .types import ASTContext


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


def group_signals_by_ast_context(
    signals: list[IntegrationSignal],
    file_reader: object = _read_file_bytes,
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
    signals_by_file: dict[str, list[IntegrationSignal]] = defaultdict(list)
    for signal in signals:
        signals_by_file[signal.match.file_path].append(signal)

    groups: dict[tuple[str, int, int], list[IntegrationSignal]] = {}
    contexts: dict[tuple[str, int, int], ASTContext] = {}

    for file_path, file_signals in signals_by_file.items():
        first_signal = file_signals[0]
        language = first_signal.match.language
        if language is None:
            continue

        file_content = file_reader(file_path)

        for signal in file_signals:
            ctx = extract_ast_context(file_content, language, signal.match.line_number)
            key = (file_path, ctx.start_line, ctx.end_line)
            groups.setdefault(key, []).append(signal)
            contexts[key] = ctx

    return [
        SignalGroup(
            ast_context=contexts[key],
            signals=tuple(group_signals),
            file_path=key[0],
        )
        for key, group_signals in groups.items()
    ]
