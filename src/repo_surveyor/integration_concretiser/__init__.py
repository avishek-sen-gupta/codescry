"""Integration signal concretisation via ML classification.

Groups raw integration signals by their enclosing AST node (function/method/class),
then classifies each signal using a trained SignalClassifier.
"""

from collections.abc import Callable

from repo_surveyor.detection.integration_detector import (
    IntegrationDetectorResult,
    IntegrationSignal,
    EntityType,
)
from repo_surveyor.training.signal_classifier import SignalClassifier
from repo_surveyor.integration_concretiser.concretiser import concretise_groups
from repo_surveyor.integration_concretiser.grouper import (
    SignalGroup,
    group_signals_by_ast_context,
)
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    ConcretisedSignal,
    ConcretisationResult,
)


def _read_file_bytes(file_path: str) -> bytes:
    with open(file_path, "rb") as f:
        return f.read()


def concretise_integration_signals(
    detector_result: IntegrationDetectorResult,
    classifier: SignalClassifier,
    file_reader: Callable[[str], bytes] = _read_file_bytes,
) -> ConcretisationResult:
    """Concretise FILE_CONTENT integration signals using ML classification.

    Filters the detector result to FILE_CONTENT signals, groups them by
    enclosing AST node, then classifies each signal line with the classifier.

    Args:
        detector_result: Output from the integration detector.
        classifier: Trained SignalClassifier to predict labels.
        file_reader: Callable that reads a file path and returns bytes.
            Defaults to reading from disk. Inject for testing.

    Returns:
        ConcretisationResult with all signals labelled.
    """
    file_content_signals: list[IntegrationSignal] = [
        s
        for s in detector_result.integration_points
        if s.entity_type == EntityType.FILE_CONTENT
    ]
    groups = group_signals_by_ast_context(file_content_signals, file_reader)
    return concretise_groups(groups, classifier)


__all__ = [
    "ASTContext",
    "ConcretisationResult",
    "ConcretisedSignal",
    "SignalGroup",
    "concretise_integration_signals",
    "group_signals_by_ast_context",
]
