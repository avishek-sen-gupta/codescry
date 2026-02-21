"""ML-based concretisation of integration signals using SignalClassifier."""

from repo_surveyor.integration_concretiser.llm_shared import (
    map_label_to_validity_direction,
)
from repo_surveyor.training.signal_classifier import SignalClassifier
from repo_surveyor.integration_concretiser.grouper import SignalGroup
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisedSignal,
    ConcretisationResult,
    SignalLike,
)


def _concretise_signal(
    signal: SignalLike,
    ast_context: ASTContext,
    classifier: SignalClassifier,
) -> ConcretisedSignal:
    label = classifier.predict(signal.match.line_content)
    validity, direction = map_label_to_validity_direction(label.value)
    return ConcretisedSignal(
        original_signal=signal,
        ast_context=ast_context,
        validity=validity,
        direction=direction,
    )


def concretise_groups(
    groups: list[SignalGroup],
    classifier: SignalClassifier,
) -> ConcretisationResult:
    """Classify each signal in each group using the ML classifier.

    Args:
        groups: Signal groups produced by group_signals_by_ast_context.
        classifier: Trained SignalClassifier to predict labels.

    Returns:
        ConcretisationResult with all signals classified.
    """
    all_signals = tuple(
        _concretise_signal(signal, group.ast_context, classifier)
        for group in groups
        for signal in group.signals
    )
    classified = sum(1 for s in all_signals if s.is_integration)
    return ConcretisationResult(
        concretised=all_signals,
        signals_submitted=len(all_signals),
        signals_classified=classified,
        signals_unclassified=len(all_signals) - classified,
    )
