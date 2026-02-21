"""Pattern-description embedding concretiser.

Classifies each FILE_CONTENT integration signal by nearest-neighbor lookup
against pre-embedded per-pattern descriptions.  Unlike the generic
``EmbeddingConcretiser`` which uses 26 directional descriptions, this
concretiser builds embedding targets from the description strings attached
to every individual regex pattern, yielding much richer semantic targets.

Usage:
    from repo_surveyor.integration_concretiser.pattern_embedding_concretiser import (
        PatternEmbeddingConcretiser,
    )
    from repo_surveyor.integration_concretiser.embedding_concretiser import (
        GeminiEmbeddingClient,
    )
    client = GeminiEmbeddingClient(api_key=...)
    concretiser = PatternEmbeddingConcretiser(client, threshold=0.56)
    result, metadata = concretiser.concretise(detector_result)
"""

import logging
from collections.abc import Callable

from repo_surveyor.detection.integration_detector import (
    EntityType,
    IntegrationDetectorResult,
)
from repo_surveyor.integration_concretiser.ast_walker import (
    FALLBACK_AST_CONTEXT,
    extract_invocation_context,
)
from repo_surveyor.integration_concretiser.embedding_concretiser import (
    cosine,
    _read_file_bytes,
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

_DEFAULT_THRESHOLD = 0.56


class PatternEmbeddingConcretiser:
    """Classify integration signals via nearest-neighbor pattern descriptions."""

    def __init__(self, client: object, threshold: float = _DEFAULT_THRESHOLD) -> None:
        self._client = client
        self._threshold = threshold
        self._descriptions = get_all_pattern_descriptions()
        desc_texts = [d.text for d in self._descriptions]
        logger.info("Pre-embedding %d pattern descriptions...", len(desc_texts))
        self._desc_embeddings: list[list[float]] = (
            client.embed_batch(desc_texts) if desc_texts else []
        )
        logger.info(
            "Pre-embedded %d pattern description embeddings",
            len(self._desc_embeddings),
        )

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
        file_content_signals: list[SignalLike] = [
            s
            for s in detector_result.integration_points
            if s.entity_type == EntityType.FILE_CONTENT
        ]

        logger.info(
            "Pattern-embedding concretiser: %d FILE_CONTENT signals",
            len(file_content_signals),
        )

        signal_contexts = self._extract_contexts(file_content_signals, file_reader)
        line_texts = [sig.match.line_content.strip() for sig in file_content_signals]

        logger.info("Embedding %d signal line texts...", len(line_texts))
        signal_embeddings = self._client.embed_batch(line_texts) if line_texts else []

        concretised, metadata = self._classify_signals(
            file_content_signals, signal_contexts, signal_embeddings
        )

        classified = sum(1 for s in concretised if s.is_integration)
        result = ConcretisationResult(
            concretised=tuple(concretised),
            signals_submitted=len(concretised),
            signals_classified=classified,
            signals_unclassified=len(concretised) - classified,
        )
        logger.info(
            "Pattern-embedding concretisation complete: "
            "submitted=%d classified=%d unclassified=%d",
            result.signals_submitted,
            result.signals_classified,
            result.signals_unclassified,
        )
        return result, metadata

    def _extract_contexts(
        self,
        signals: list[SignalLike],
        file_reader: Callable[[str], bytes],
    ) -> list[ASTContext]:
        """Extract statement-level AST context for each signal."""
        logger.info("Extracting AST contexts for %d signals...", len(signals))
        file_cache: dict[str, bytes] = {}
        contexts: list[ASTContext] = []
        fallback_count = 0
        for sig in signals:
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
                fallback_count += 1
                contexts.append(FALLBACK_AST_CONTEXT)
                continue

            ctx = extract_invocation_context(
                file_cache[fp], language, sig.match.line_number
            )
            contexts.append(ctx)

        logger.info(
            "AST context extraction complete: %d contexts from %d unique files "
            "(%d fallbacks)",
            len(contexts),
            len(file_cache),
            fallback_count,
        )
        return contexts

    def _classify_signals(
        self,
        signals: list[SignalLike],
        contexts: list[ASTContext],
        embeddings: list[list[float]],
    ) -> tuple[list[ConcretisedSignal], dict[tuple[str, int], dict]]:
        """Classify each signal by nearest-neighbor description lookup."""
        logger.info(
            "Classifying %d signals against %d pattern descriptions "
            "(threshold=%.3f)",
            len(signals),
            len(self._descriptions),
            self._threshold,
        )
        concretised: list[ConcretisedSignal] = []
        metadata: dict[tuple[str, int], dict] = {}
        label_counts: dict[str, int] = {}

        for idx, (sig, ctx, emb) in enumerate(zip(signals, contexts, embeddings)):
            best_desc, best_score = self._find_nearest(emb)

            if best_score < self._threshold:
                validity = SignalValidity.NOISE
                direction = SignalDirection.AMBIGUOUS
            else:
                validity = SignalValidity.SIGNAL
                direction = best_desc.direction

            tag = f"{validity.value}:{direction.value}"
            label_counts[tag] = label_counts.get(tag, 0) + 1

            key = (sig.match.file_path, sig.match.line_number)
            metadata[key] = {
                "nearest_description": best_desc.text,
                "nearest_type": best_desc.integration_type.value,
                "nearest_direction": best_desc.direction.value,
                "nearest_source": best_desc.source,
                "score": best_score,
            }

            logger.debug(
                "  [%d/%d] %s:%d  score=%.3f  type=%s  dir=%s  src=%s",
                idx + 1,
                len(signals),
                sig.match.file_path,
                sig.match.line_number,
                best_score,
                best_desc.integration_type.value,
                best_desc.direction.value,
                best_desc.source,
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
