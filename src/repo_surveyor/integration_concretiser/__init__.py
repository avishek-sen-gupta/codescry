"""Integration signal concretisation: classify signals as definite/not and inward/outward.

Public API for concretising raw integration signals into definite
integration points with direction, using AST context and LLM classification.
"""

from ..integration_detector import EntityType, IntegrationDetectorResult
from ..ml_classifier.model_protocol import LineClassifierModel
from .concretiser import concretise_groups
from .grouper import group_signals_by_ast_context
from .types import ConcretisationResult


def concretise_integration_signals(
    detector_result: IntegrationDetectorResult,
    model: LineClassifierModel,
    file_reader: object = None,
) -> ConcretisationResult:
    """Concretise integration signals from a detector result.

    Pipeline: filter to FILE_CONTENT signals only -> group by AST context
    -> concretise via LLM -> return aggregated result.

    Args:
        detector_result: Raw integration detection result.
        model: LLM model implementing LineClassifierModel protocol.
        file_reader: Optional callable to read file bytes. Defaults to
            reading from disk. Inject for testing.

    Returns:
        ConcretisationResult with classified signals and summary counts.
    """
    file_content_signals = [
        signal
        for signal in detector_result.integration_points
        if signal.entity_type == EntityType.FILE_CONTENT
    ]

    kwargs = {}
    if file_reader is not None:
        kwargs["file_reader"] = file_reader

    groups = group_signals_by_ast_context(file_content_signals, **kwargs)
    return concretise_groups(groups, model)
