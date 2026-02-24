"""Orchestrate evidence predicate evaluation for integration signals.

Evaluates each signal against its checklist of predicates, computes a
weighted score adjustment, and returns an EvidenceVerdict with the
adjusted score clamped to [0.0, 1.0].
"""

import logging

from repo_surveyor.integration_concretiser.checklist_registry import (
    ChecklistRegistry,
)
from repo_surveyor.integration_concretiser.evidence_predicates import (
    ChecklistEntry,
    EvidenceVerdict,
    PredicateResult,
)
from repo_surveyor.integration_concretiser.predicate_context_builder import (
    PredicateContext,
)
from repo_surveyor.integration_concretiser.predicates import PREDICATE_DISPATCH
from repo_surveyor.integration_concretiser.types import SignalLike

logger = logging.getLogger(__name__)

_EMPTY_VERDICT_RESULTS: tuple[PredicateResult, ...] = ()


def _clamp(value: float, lo: float, hi: float) -> float:
    """Clamp a value to [lo, hi]."""
    return max(lo, min(hi, value))


def _evaluate_checklist(
    checklist: tuple[ChecklistEntry, ...],
    ctx: PredicateContext,
    original_score: float,
) -> EvidenceVerdict:
    """Evaluate all predicates in a checklist against a single signal.

    Args:
        checklist: The predicate entries to evaluate.
        ctx: Pre-computed context for the signal.
        original_score: Raw embedding similarity score.

    Returns:
        EvidenceVerdict with individual results and adjusted score.
    """
    if not checklist:
        return EvidenceVerdict(
            predicate_results=_EMPTY_VERDICT_RESULTS,
            score_adjustment=0.0,
            original_score=original_score,
            adjusted_score=original_score,
        )

    results: list[PredicateResult] = []
    adjustment = 0.0

    for entry in checklist:
        predicate_fn = PREDICATE_DISPATCH.get(entry.predicate)
        if predicate_fn is None:
            logger.warning("Unknown predicate %s — skipping", entry.predicate)
            continue

        matched = predicate_fn(ctx, entry.pattern_arg)
        results.append(
            PredicateResult(
                predicate_name=entry.predicate,
                matched=matched,
                weight=entry.weight,
            )
        )
        if matched:
            adjustment += entry.weight

    adjusted = _clamp(original_score + adjustment, 0.0, 1.0)
    return EvidenceVerdict(
        predicate_results=tuple(results),
        score_adjustment=adjustment,
        original_score=original_score,
        adjusted_score=adjusted,
    )


class EvidenceEvaluator:
    """Orchestrate per-signal evidence evaluation using a checklist registry.

    Usage::

        registry = ChecklistRegistry(Path("data/evidence_checklists/default.json"))
        evaluator = EvidenceEvaluator(registry)
        verdict = evaluator.evaluate(signal, predicate_ctx, raw_score=0.65)
    """

    def __init__(self, registry: ChecklistRegistry) -> None:
        self._registry = registry

    def evaluate(
        self,
        signal: SignalLike,
        ctx: PredicateContext,
        raw_score: float,
    ) -> EvidenceVerdict:
        """Evaluate evidence predicates for a single signal.

        Looks up the appropriate checklist for the signal's source and
        matched_pattern, then evaluates all predicates against the context.

        Args:
            signal: The integration signal being verified.
            ctx: Pre-computed predicate context for the signal.
            raw_score: Raw embedding similarity score.

        Returns:
            EvidenceVerdict with adjusted score.
        """
        checklist = self._registry.get_checklist(signal.source, signal.matched_pattern)
        return _evaluate_checklist(checklist, ctx, raw_score)

    def batch_evaluate(
        self,
        signals: list[SignalLike],
        contexts: list[PredicateContext],
        raw_scores: list[float],
    ) -> list[EvidenceVerdict]:
        """Evaluate evidence predicates for a batch of signals.

        Args:
            signals: Integration signals to verify.
            contexts: Pre-computed predicate contexts (same order as signals).
            raw_scores: Raw embedding similarity scores (same order).

        Returns:
            List of EvidenceVerdict objects, one per signal.
        """
        logger.info("Evaluating evidence predicates for %d signals...", len(signals))
        verdicts = [
            self.evaluate(sig, ctx, score)
            for sig, ctx, score in zip(signals, contexts, raw_scores)
        ]
        fired_count = sum(
            1 for v in verdicts if any(r.matched for r in v.predicate_results)
        )
        logger.info(
            "Evidence evaluation complete: %d/%d signals had predicates fire",
            fired_count,
            len(verdicts),
        )
        return verdicts
