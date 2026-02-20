"""ML-based concretisation of integration signals using SignalClassifier."""

from ..training.signal_classifier import SignalClassifier
from ..detection.integration_detector import IntegrationSignal
from .grouper import SignalGroup
from .types import ASTContext, ConcretisedSignal, ConcretisationResult


def _concretise_signal(
    signal: IntegrationSignal,
    ast_context: ASTContext,
    classifier: SignalClassifier,
) -> ConcretisedSignal:
    label = classifier.predict(signal.match.line_content)
    return ConcretisedSignal(
        original_signal=signal,
        ast_context=ast_context,
        label=label,
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
        ConcretisationResult with all signals labelled.
    """
    all_signals = tuple(
        _concretise_signal(signal, group.ast_context, classifier)
        for group in groups
        for signal in group.signals
    )
    definite = sum(1 for s in all_signals if s.is_definite)
    return ConcretisationResult(
        concretised=all_signals,
        signals_submitted=len(all_signals),
        signals_definite=definite,
        signals_discarded=len(all_signals) - definite,
    )
