"""Integration signal concretisation: classify signals as definite/not and inward/outward.

Public API for concretising raw integration signals into definite
integration points with direction, using AST context and LLM classification.
"""

import logging
from collections.abc import Callable

from ..integration_detector import EntityType, IntegrationDetectorResult
from ..ml_classifier.model_protocol import LineClassifierModel
from .concretiser import concretise_groups
from .grouper import _read_file_bytes, group_signals_by_ast_context
from .types import ConcretisationResult

logger = logging.getLogger(__name__)


def concretise_integration_signals(
    detector_result: IntegrationDetectorResult,
    model: LineClassifierModel,
    file_reader: Callable[[str], bytes] = _read_file_bytes,
    batch_size: int = 10,
) -> ConcretisationResult:
    """Concretise integration signals from a detector result.

    Pipeline: filter to FILE_CONTENT signals only -> group by AST context
    -> concretise via LLM -> return aggregated result.

    Args:
        detector_result: Raw integration detection result.
        model: LLM model implementing LineClassifierModel protocol.
        file_reader: Callable to read file bytes. Defaults to reading
            from disk. Inject for testing.
        batch_size: Number of signal groups per LLM call. Defaults to 10.

    Returns:
        ConcretisationResult with classified signals and summary counts.
    """
    logger.info(
        "Starting concretisation: %d total signals",
        len(detector_result.integration_points),
    )

    file_content_signals = [
        signal
        for signal in detector_result.integration_points
        if signal.entity_type == EntityType.FILE_CONTENT
    ]
    logger.info(
        "Filtered to %d FILE_CONTENT signals (discarded %d non-file signals)",
        len(file_content_signals),
        len(detector_result.integration_points) - len(file_content_signals),
    )

    groups = group_signals_by_ast_context(file_content_signals, file_reader)
    logger.info(
        "Grouped into %d AST context groups",
        len(groups),
    )

    return concretise_groups(groups, model, batch_size=batch_size)
