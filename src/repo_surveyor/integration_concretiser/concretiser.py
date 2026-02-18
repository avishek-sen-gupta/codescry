"""LLM-based concretisation of integration signal groups."""

from itertools import chain

from ..ml_classifier.model_protocol import LineClassifierModel
from .grouper import SignalGroup
from .prompt import build_system_prompt, build_user_prompt
from .response_parser import parse_response
from .types import ConcretisationResult, ConcretisedSignal


def _concretise_group(
    group: SignalGroup,
    model: LineClassifierModel,
    system_prompt: str,
) -> tuple[ConcretisedSignal, ...]:
    """Concretise a single signal group via one LLM call."""
    user_prompt = build_user_prompt(group)
    result = model.classify(system_prompt, user_prompt)
    return parse_response(result.text, group)


def concretise_groups(
    groups: list[SignalGroup],
    model: LineClassifierModel,
) -> ConcretisationResult:
    """Concretise all signal groups via LLM calls.

    Makes one LLM call per signal group and aggregates the results.

    Args:
        groups: Signal groups to concretise.
        model: LLM model implementing the LineClassifierModel protocol.

    Returns:
        ConcretisationResult with all concretised signals and counts.
    """
    system_prompt = build_system_prompt()

    all_signals = tuple(
        chain.from_iterable(
            _concretise_group(group, model, system_prompt) for group in groups
        )
    )

    signals_definite = sum(1 for s in all_signals if s.is_definite)

    return ConcretisationResult(
        concretised=all_signals,
        signals_submitted=len(all_signals),
        signals_definite=signals_definite,
        signals_discarded=len(all_signals) - signals_definite,
    )
