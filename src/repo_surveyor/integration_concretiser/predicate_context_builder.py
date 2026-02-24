"""Build PredicateContext objects for evidence predicate evaluation.

Reuses the file cache and ASTContexts already computed by the embedding
concretiser to avoid redundant I/O and parsing.  For ancestor node types,
performs one tree-sitter parse per file and walks from the deepest node
at each signal line upward, collecting node types.
"""

import logging
from dataclasses import dataclass

from repo_surveyor.integration_concretiser.types import ASTContext, SignalLike
from repo_surveyor.integration_patterns import Language
from repo_surveyor.detection.syntax_zone import LANGUAGE_TO_TS_NAME

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class PredicateContext:
    """Pre-computed data for evaluating predicates against a single signal.

    Carries structural (AST), file-level, and textual data so predicates
    can inspect any dimension without re-parsing or re-reading files.

    Attributes:
        file_path: Path to the source file containing the signal.
        line_number: 1-indexed line number of the signal.
        line_content: Stripped content of the signal line.
        language: Programming language (None if unknown).
        source_lines: All lines of the file as strings (for sibling checks).
        enclosing_node_text: Full text of the enclosing AST node.
        enclosing_node_type: Tree-sitter node type of the enclosing AST node.
        ancestor_node_types: Node types from deepest node at signal line up
                             to the enclosing statement (for IN_STRING_CONTEXT).
    """

    file_path: str
    line_number: int
    line_content: str
    language: Language | None
    source_lines: tuple[str, ...]
    enclosing_node_text: str
    enclosing_node_type: str
    ancestor_node_types: tuple[str, ...]


def _collect_ancestor_types(
    file_content: bytes,
    language: Language,
    line_numbers: frozenset[int],
) -> dict[int, tuple[str, ...]]:
    """Collect ancestor node types for each line in a single parse.

    Walks from the deepest named node at each line upward to the root,
    collecting all node types encountered.

    Args:
        file_content: Raw bytes of the source file.
        language: Programming language of the file.
        line_numbers: Set of 1-indexed line numbers to resolve.

    Returns:
        Mapping from 1-indexed line number to tuple of ancestor node types
        (deepest first, root last).  Lines with no AST coverage map to
        empty tuples.
    """
    ts_name = LANGUAGE_TO_TS_NAME.get(language)
    if ts_name is None:
        return {ln: () for ln in line_numbers}

    from tree_sitter_language_pack import get_parser

    parser = get_parser(ts_name)
    tree = parser.parse(file_content)

    from repo_surveyor.integration_concretiser.ast_walker import (
        _deepest_named_nodes_at_lines,
    )

    lines_0indexed = frozenset(ln - 1 for ln in line_numbers)
    deepest_by_line = _deepest_named_nodes_at_lines(tree.root_node, lines_0indexed)

    results: dict[int, tuple[str, ...]] = {}
    for ln in line_numbers:
        line_0 = ln - 1
        deepest = deepest_by_line.get(line_0)
        if deepest is None:
            results[ln] = ()
            continue
        types: list[str] = []
        current = deepest
        while current is not None:
            types.append(current.type)
            current = current.parent
        results[ln] = tuple(types)

    return results


def batch_build_predicate_contexts(
    signals: list[SignalLike],
    ast_contexts: list[ASTContext],
    file_cache: dict[str, bytes],
) -> list[PredicateContext]:
    """Build PredicateContext objects for a batch of signals.

    Reuses the file cache populated during AST context extraction to
    avoid re-reading files.  Computes ancestor node types via one
    tree-sitter parse per (file, language) pair.

    Args:
        signals: Integration signals to build contexts for.
        ast_contexts: Corresponding ASTContext for each signal.
        file_cache: Mapping from file path to raw bytes (already populated).

    Returns:
        List of PredicateContext objects, one per signal, in the same order.
    """
    logger.info("Building predicate contexts for %d signals...", len(signals))

    # Pre-compute source lines per file (decoded once)
    source_lines_cache: dict[str, tuple[str, ...]] = {}
    for fp, content in file_cache.items():
        source_lines_cache[fp] = tuple(
            content.decode("utf-8", errors="replace").splitlines()
        )

    # Group signals by (file_path, language) for batch ancestor extraction
    groups: dict[tuple[str, Language], list[int]] = {}
    for idx, sig in enumerate(signals):
        lang = sig.match.language
        if lang is not None:
            key = (sig.match.file_path, lang)
            groups.setdefault(key, []).append(idx)

    # Batch collect ancestor types per group
    ancestor_types_by_index: dict[int, tuple[str, ...]] = {}
    for (fp, lang), indices in groups.items():
        line_numbers = frozenset(signals[idx].match.line_number for idx in indices)
        content = file_cache.get(fp, b"")
        ancestors_by_line = _collect_ancestor_types(content, lang, line_numbers)
        for idx in indices:
            ln = signals[idx].match.line_number
            ancestor_types_by_index[idx] = ancestors_by_line.get(ln, ())

    # Build PredicateContext per signal
    contexts: list[PredicateContext] = []
    for idx, (sig, ast_ctx) in enumerate(zip(signals, ast_contexts)):
        fp = sig.match.file_path
        source_lines = source_lines_cache.get(fp, ())
        contexts.append(
            PredicateContext(
                file_path=fp,
                line_number=sig.match.line_number,
                line_content=sig.match.line_content.strip(),
                language=sig.match.language,
                source_lines=source_lines,
                enclosing_node_text=ast_ctx.node_text,
                enclosing_node_type=ast_ctx.node_type,
                ancestor_node_types=ancestor_types_by_index.get(idx, ()),
            )
        )

    logger.info(
        "Predicate context construction complete: %d contexts, %d unique files",
        len(contexts),
        len(source_lines_cache),
    )
    return contexts
