"""LLM-based concretisation of integration signal groups."""

import logging
from itertools import chain

from ..ml_classifier.model_protocol import LineClassifierModel
from .grouper import SignalGroup
from .prompt import (
    build_batched_system_prompt,
    build_batched_user_prompt,
    build_system_prompt,
    build_user_prompt,
)
from .response_parser import parse_batched_response, parse_response
from .types import ConcretisationResult, ConcretisedSignal

logger = logging.getLogger(__name__)


class _BatchConfig:
    """Configuration for batching LLM calls."""

    DEFAULT_BATCH_SIZE = 10


def _concretise_group(
    group: SignalGroup,
    model: LineClassifierModel,
    system_prompt: str,
) -> tuple[ConcretisedSignal, ...]:
    """Concretise a single signal group via one LLM call."""
    user_prompt = build_user_prompt(group)
    result = model.classify(system_prompt, user_prompt)
    return parse_response(result.text, group)


def _concretise_batch(
    groups: list[SignalGroup],
    model: LineClassifierModel,
    system_prompt: str,
) -> tuple[ConcretisedSignal, ...]:
    """Concretise a batch of signal groups via one LLM call."""
    user_prompt = build_batched_user_prompt(groups)
    result = model.classify(system_prompt, user_prompt)
    per_group = parse_batched_response(result.text, groups)
    return tuple(chain.from_iterable(per_group))


def _partition_into_batches(
    groups: list[SignalGroup],
    batch_size: int,
) -> list[list[SignalGroup]]:
    """Partition groups into batches of at most batch_size."""
    return [groups[i : i + batch_size] for i in range(0, len(groups), batch_size)]


def concretise_groups(
    groups: list[SignalGroup],
    model: LineClassifierModel,
    batch_size: int = _BatchConfig.DEFAULT_BATCH_SIZE,
) -> ConcretisationResult:
    """Concretise all signal groups via LLM calls.

    Groups are batched into multi-group LLM calls to reduce overhead.
    Single-group batches fall back to the unbatched path for simplicity.

    Args:
        groups: Signal groups to concretise.
        model: LLM model implementing the LineClassifierModel protocol.
        batch_size: Number of groups per LLM call. Defaults to 10.

    Returns:
        ConcretisationResult with all concretised signals and counts.
    """
    total_signals = sum(len(g.signals) for g in groups)
    logger.info(
        "Concretising %d signal groups (%d signals) with batch_size=%d",
        len(groups),
        total_signals,
        batch_size,
    )

    batches = _partition_into_batches(groups, batch_size)
    logger.info("Partitioned into %d LLM batches", len(batches))

    batched_system_prompt = build_batched_system_prompt()
    single_system_prompt = build_system_prompt()

    all_signals: list[ConcretisedSignal] = []
    for batch_index, batch in enumerate(batches):
        batch_signal_count = sum(len(g.signals) for g in batch)
        logger.info(
            "Processing batch %d/%d (%d groups, %d signals)",
            batch_index + 1,
            len(batches),
            len(batch),
            batch_signal_count,
        )

        if len(batch) == 1:
            batch_results = _concretise_group(batch[0], model, single_system_prompt)
        else:
            batch_results = _concretise_batch(batch, model, batched_system_prompt)

        definite_in_batch = sum(1 for s in batch_results if s.is_definite)
        logger.info(
            "Batch %d/%d complete: %d definite, %d discarded",
            batch_index + 1,
            len(batches),
            definite_in_batch,
            len(batch_results) - definite_in_batch,
        )
        all_signals.extend(batch_results)

    result_tuple = tuple(all_signals)
    signals_definite = sum(1 for s in result_tuple if s.is_definite)

    logger.info(
        "Concretisation complete: %d/%d definite, %d discarded",
        signals_definite,
        len(result_tuple),
        len(result_tuple) - signals_definite,
    )

    return ConcretisationResult(
        concretised=result_tuple,
        signals_submitted=len(result_tuple),
        signals_definite=signals_definite,
        signals_discarded=len(result_tuple) - signals_definite,
    )
