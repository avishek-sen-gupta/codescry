"""Pattern-description embedding concretiser.

Classifies each FILE_CONTENT integration signal by nearest-neighbor lookup
against pre-embedded per-pattern descriptions.  Unlike the generic
``EmbeddingConcretiser`` which uses 26 directional descriptions, this
concretiser builds embedding targets from the description strings attached
to every individual regex pattern, yielding much richer semantic targets.

Supports caching of the static pattern description embeddings to avoid
redundant API calls on repeated runs.  The cache is keyed by a SHA-256
content hash of all description texts, so it auto-invalidates when
patterns change.

Usage:
    from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
        PatternEmbeddingConcretiser,
    )
    from repo_surveyor.integration_concretiser.embedding_concretiser import (
        GeminiEmbeddingClient,
    )
    client = GeminiEmbeddingClient(api_key=...)
    concretiser = PatternEmbeddingConcretiser(client, threshold=0.62, cache_path=Path("data/embeddings/cache.json"))
    result, metadata = concretiser.concretise(detector_result)
"""

import hashlib
import json
import logging
import time
from collections.abc import Callable
from pathlib import Path

from repo_surveyor.detection.integration_detector import (
    EntityType,
    IntegrationDetectorResult,
)
from repo_surveyor.integration_concretiser.ast_walker import (
    FALLBACK_AST_CONTEXT,
    batch_extract_statement_contexts,
)
from repo_surveyor.integration_concretiser.embedding_concretiser import (
    EmbeddingClientProtocol,
    cosine,
    _read_file_bytes,
)
from repo_surveyor.integration_concretiser.checklist_registry import (
    ChecklistRegistry,
)
from repo_surveyor.integration_concretiser.evidence_evaluator import (
    EvidenceEvaluator,
)
from repo_surveyor.integration_concretiser.evidence_predicates import (
    EvidenceVerdict,
)
from repo_surveyor.integration_concretiser.predicate_context_builder import (
    PredicateContext,
    batch_build_predicate_contexts,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisationResult,
    ConcretisedSignal,
    SignalLike,
    SignalValidity,
)
from repo_surveyor.integration_patterns import (
    PatternDescription,
    SignalDirection,
    get_all_pattern_descriptions,
)

logger = logging.getLogger(__name__)

_DEFAULT_THRESHOLD = 0.62

_EMPTY_PATH = Path()


def _default_cache_path(backend: str, model: str = "") -> Path:
    """Return the conventional cache file path for a given backend.

    When *model* is provided, the sanitised model name (``/`` replaced with
    ``--``) is appended to distinguish caches from different models within the
    same backend.  When *model* is empty, the filename is unchanged for
    backwards compatibility with backends that don't expose a model choice.
    """
    if model:
        sanitised = model.replace("/", "--")
        return Path(
            f"data/embeddings/pattern_description_embeddings_{backend}_{sanitised}.json"
        )
    return Path(f"data/embeddings/pattern_description_embeddings_{backend}.json")


def _compute_content_hash(descriptions: tuple[PatternDescription, ...]) -> str:
    """Compute a SHA-256 hash over all description texts for cache invalidation."""
    joined = "\n".join(d.text for d in descriptions)
    return hashlib.sha256(joined.encode("utf-8")).hexdigest()


def _load_cached_embeddings(
    cache_path: Path, descriptions: tuple[PatternDescription, ...]
) -> list[list[float]]:
    """Load cached embeddings if the cache file exists and matches current descriptions.

    Args:
        cache_path: Path to the JSON cache file.
        descriptions: Current pattern descriptions for hash validation.

    Returns:
        List of embedding vectors on cache hit, empty list on cache miss.
    """
    if not cache_path.exists():
        return []

    try:
        data = json.loads(cache_path.read_text(encoding="utf-8"))
    except (json.JSONDecodeError, OSError) as exc:
        logger.warning("Failed to read cache file %s: %s", cache_path, exc)
        return []

    expected_hash = _compute_content_hash(descriptions)
    if data.get("content_hash") != expected_hash:
        logger.info(
            "Cache content hash mismatch (expected %s, got %s) — rebuilding",
            expected_hash,
            data.get("content_hash"),
        )
        return []

    if data.get("description_count") != len(descriptions):
        logger.info(
            "Cache description count mismatch (expected %d, got %s) — rebuilding",
            len(descriptions),
            data.get("description_count"),
        )
        return []

    embeddings = data.get("embeddings", [])
    if len(embeddings) != len(descriptions):
        logger.info(
            "Cache embeddings count mismatch (expected %d, got %d) — rebuilding",
            len(descriptions),
            len(embeddings),
        )
        return []

    return embeddings


def _save_cache(
    cache_path: Path,
    descriptions: tuple[PatternDescription, ...],
    embeddings: list[list[float]],
    backend: str = "",
) -> None:
    """Save pattern description embeddings to a JSON cache file.

    Args:
        cache_path: Path to write the cache file.
        descriptions: The pattern descriptions that were embedded.
        embeddings: The embedding vectors to cache.
        backend: Name of the embedding backend used.
    """
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    data = {
        "backend": backend,
        "content_hash": _compute_content_hash(descriptions),
        "description_count": len(descriptions),
        "embedding_dim": len(embeddings[0]) if embeddings else 0,
        "descriptions": [
            {
                "text": d.text,
                "integration_type": d.integration_type.value,
                "direction": d.direction.value,
                "source": d.source,
            }
            for d in descriptions
        ],
        "embeddings": embeddings,
    }
    cache_path.write_text(json.dumps(data), encoding="utf-8")
    logger.info("Saved embedding cache to %s", cache_path)


class PatternEmbeddingConcretiser:
    """Classify integration signals via nearest-neighbor pattern descriptions."""

    def __init__(
        self,
        client: EmbeddingClientProtocol,
        threshold: float = _DEFAULT_THRESHOLD,
        cache_path: Path = _EMPTY_PATH,
        checklist_path: Path = _EMPTY_PATH,
    ) -> None:
        self._client = client
        self._threshold = threshold
        self._descriptions = get_all_pattern_descriptions()
        self._desc_embeddings = self._load_or_embed(cache_path)
        self._evaluator = self._build_evaluator(checklist_path)

    def _load_or_embed(self, cache_path: Path) -> list[list[float]]:
        """Load cached description embeddings or embed via client and save."""
        if cache_path != _EMPTY_PATH:
            cached = _load_cached_embeddings(cache_path, self._descriptions)
            if cached:
                logger.info(
                    "Using cached embeddings (%d vectors) from %s",
                    len(cached),
                    cache_path,
                )
                return cached

        desc_texts = [d.text for d in self._descriptions]
        logger.info("Pre-embedding %d pattern descriptions...", len(desc_texts))
        embeddings: list[list[float]] = (
            self._client.embed_batch(desc_texts) if desc_texts else []
        )
        logger.info(
            "Pre-embedded %d pattern description embeddings",
            len(embeddings),
        )

        if cache_path != _EMPTY_PATH and embeddings:
            _save_cache(cache_path, self._descriptions, embeddings)

        return embeddings

    def _build_evaluator(self, checklist_path: Path) -> EvidenceEvaluator | None:
        """Build an evidence evaluator from the checklist path, or None if disabled."""
        if checklist_path == _EMPTY_PATH:
            return None
        registry = ChecklistRegistry(checklist_path)
        logger.info("Evidence evaluation enabled via %s", checklist_path)
        return EvidenceEvaluator(registry)

    def concretise(
        self,
        detector_result: IntegrationDetectorResult,
        file_reader: Callable[[str], bytes] = _read_file_bytes,
    ) -> tuple[ConcretisationResult, dict[tuple[str, int], dict]]:
        """Concretise FILE_CONTENT signals using nearest pattern description.

        Args:
            detector_result: Output from the integration detector.
            file_reader: Callable that reads a file path to bytes.

        Returns:
            Tuple of (ConcretisationResult, metadata) where metadata maps
            (file_path, line_number) to nearest description info and score.
        """
        t0_all = time.monotonic()
        file_content_signals: list[SignalLike] = [
            s
            for s in detector_result.integration_points
            if s.entity_type == EntityType.FILE_CONTENT
        ]

        logger.info(
            "Pattern-embedding concretiser: %d FILE_CONTENT signals",
            len(file_content_signals),
        )

        signal_contexts, predicate_contexts = self._extract_contexts(
            file_content_signals, file_reader
        )
        embed_texts = [
            ctx.node_text if ctx.node_text else sig.match.line_content.strip()
            for sig, ctx in zip(file_content_signals, signal_contexts)
        ]

        logger.info("Embedding %d signal texts (with AST context)...", len(embed_texts))
        signal_embeddings = self._client.embed_batch(embed_texts) if embed_texts else []

        concretised, metadata = self._classify_signals(
            file_content_signals,
            signal_contexts,
            signal_embeddings,
            predicate_contexts,
        )

        classified = sum(1 for s in concretised if s.is_integration)
        result = ConcretisationResult(
            concretised=tuple(concretised),
            signals_submitted=len(concretised),
            signals_classified=classified,
            signals_unclassified=len(concretised) - classified,
        )
        elapsed_total = time.monotonic() - t0_all
        logger.info(
            "Pattern-embedding concretisation complete: "
            "submitted=%d classified=%d unclassified=%d elapsed=%.2fs",
            result.signals_submitted,
            result.signals_classified,
            result.signals_unclassified,
            elapsed_total,
        )
        return result, metadata

    def _extract_contexts(
        self,
        signals: list[SignalLike],
        file_reader: Callable[[str], bytes],
    ) -> tuple[list[ASTContext], list[PredicateContext]]:
        """Extract statement-level AST context and predicate context for each signal.

        Groups signals by file and language, then calls
        batch_extract_statement_contexts once per (file, language) pair
        to avoid redundant parses and AST walks.  When evidence evaluation
        is enabled, additionally builds PredicateContext objects reusing
        the same file cache.

        Returns:
            Tuple of (ast_contexts, predicate_contexts).  When evidence
            evaluation is disabled, predicate_contexts is an empty list.
        """
        logger.info("Extracting AST contexts for %d signals...", len(signals))
        file_cache: dict[str, bytes] = {}
        # Group signals by (file_path, language) for batch extraction
        groups: dict[tuple[str, Language], list[int]] = {}
        fallback_indices: list[int] = []

        for idx, sig in enumerate(signals):
            fp = sig.match.file_path
            if fp not in file_cache:
                try:
                    file_cache[fp] = file_reader(fp)
                    logger.debug("Cached file: %s", fp)
                except OSError as exc:
                    logger.error("Cannot read %s: %s", fp, exc)
                    file_cache[fp] = b""

            language = sig.match.language
            if language is None:
                fallback_indices.append(idx)
                continue

            key = (fp, language)
            groups.setdefault(key, []).append(idx)

        # Batch extract per (file, language) group
        context_by_index: dict[int, ASTContext] = {
            idx: FALLBACK_AST_CONTEXT for idx in fallback_indices
        }
        for (fp, language), indices in groups.items():
            line_numbers = frozenset(signals[idx].match.line_number for idx in indices)
            contexts_by_line = batch_extract_statement_contexts(
                file_cache[fp], language, line_numbers
            )
            for idx in indices:
                context_by_index[idx] = contexts_by_line[signals[idx].match.line_number]

        ast_contexts = [context_by_index[i] for i in range(len(signals))]
        fallback_count = len(fallback_indices)
        logger.info(
            "AST context extraction complete: %d contexts from %d unique files "
            "(%d fallbacks)",
            len(ast_contexts),
            len(file_cache),
            fallback_count,
        )

        # Build predicate contexts only when evidence evaluation is enabled
        predicate_contexts: list[PredicateContext] = (
            batch_build_predicate_contexts(signals, ast_contexts, file_cache)
            if self._evaluator is not None
            else []
        )

        return ast_contexts, predicate_contexts

    def _classify_signals(
        self,
        signals: list[SignalLike],
        contexts: list[ASTContext],
        embeddings: list[list[float]],
        predicate_contexts: list[PredicateContext] = (),
    ) -> tuple[list[ConcretisedSignal], dict[tuple[str, int], dict]]:
        """Classify each signal by nearest-neighbor description lookup.

        When an evidence evaluator is configured, the raw cosine similarity
        score is adjusted by predicate weights before comparing against the
        threshold.  Evidence data is included in the metadata dict under
        an ``"evidence"`` key.
        """
        logger.info(
            "Classifying %d signals against %d pattern descriptions "
            "(threshold=%.3f, evidence=%s)",
            len(signals),
            len(self._descriptions),
            self._threshold,
            "enabled" if self._evaluator is not None else "disabled",
        )
        concretised: list[ConcretisedSignal] = []
        metadata: dict[tuple[str, int], dict] = {}
        label_counts: dict[str, int] = {}
        total = len(signals)
        t0_classify = time.monotonic()
        progress_interval = max(1, total // 10)

        for idx, (sig, ctx, emb) in enumerate(zip(signals, contexts, embeddings)):
            best_desc, raw_score = self._find_nearest(emb)

            # Apply evidence evaluation when enabled
            verdict: EvidenceVerdict | None = None
            effective_score = raw_score
            if self._evaluator is not None and predicate_contexts:
                verdict = self._evaluator.evaluate(
                    sig, predicate_contexts[idx], raw_score
                )
                effective_score = verdict.adjusted_score

            if effective_score < self._threshold:
                validity = SignalValidity.NOISE
                direction = SignalDirection.AMBIGUOUS
            else:
                validity = SignalValidity.SIGNAL
                direction = best_desc.direction

            tag = f"{validity.value}:{direction.value}"
            label_counts[tag] = label_counts.get(tag, 0) + 1

            key = (sig.match.file_path, sig.match.line_number)
            meta: dict = {
                "nearest_description": best_desc.text,
                "nearest_type": best_desc.integration_type.value,
                "nearest_direction": best_desc.direction.value,
                "nearest_source": best_desc.source,
                "score": effective_score,
            }
            if verdict is not None:
                meta["evidence"] = {
                    "raw_score": verdict.original_score,
                    "adjustment": verdict.score_adjustment,
                    "predicates_fired": [
                        r.predicate_name.value
                        for r in verdict.predicate_results
                        if r.matched
                    ],
                }
            metadata[key] = meta

            logger.debug(
                "  [%d/%d] %s:%d  score=%.3f%s  type=%s  dir=%s  src=%s",
                idx + 1,
                total,
                sig.match.file_path,
                sig.match.line_number,
                effective_score,
                (
                    f" (raw={raw_score:.3f} adj={verdict.score_adjustment:+.3f})"
                    if verdict is not None
                    else ""
                ),
                best_desc.integration_type.value,
                best_desc.direction.value,
                best_desc.source,
            )

            if (idx + 1) % progress_interval == 0 or idx + 1 == total:
                logger.info(
                    "Classification progress: %d/%d signals (%.2fs elapsed)",
                    idx + 1,
                    total,
                    time.monotonic() - t0_classify,
                )

            concretised.append(
                ConcretisedSignal(
                    original_signal=sig,
                    ast_context=ctx,
                    validity=validity,
                    direction=direction,
                )
            )

        logger.info(
            "Classification complete: %s",
            ", ".join(f"{k}={v}" for k, v in sorted(label_counts.items())),
        )
        return concretised, metadata

    def _find_nearest(self, embedding: list[float]) -> tuple[PatternDescription, float]:
        """Find the nearest pattern description by cosine similarity.

        Args:
            embedding: The signal's embedding vector.

        Returns:
            Tuple of (best matching PatternDescription, similarity score).
        """
        best_idx = 0
        best_score = -1.0
        for i, desc_emb in enumerate(self._desc_embeddings):
            score = cosine(embedding, desc_emb)
            if score > best_score:
                best_score = score
                best_idx = i
        return self._descriptions[best_idx], best_score
