"""Parse LLM responses into ConcretisedSignal objects."""

from ..integration_detector import IntegrationSignal
from .grouper import SignalGroup
from .types import ASTContext, ConcretisedSignal, IntegrationDirection

_DIRECTION_MAP = {
    "INWARD": IntegrationDirection.INWARD,
    "OUTWARD": IntegrationDirection.OUTWARD,
}


def _make_not_definite(
    signal: IntegrationSignal,
    ast_context: ASTContext,
    reasoning: str,
) -> ConcretisedSignal:
    """Create a not-definite ConcretisedSignal."""
    return ConcretisedSignal(
        original_signal=signal,
        ast_context=ast_context,
        is_definite=False,
        direction=None,
        reasoning=reasoning,
    )


def _parse_single_line(line: str) -> tuple[int, bool, IntegrationDirection | None, str]:
    """Parse a single response line.

    Returns:
        Tuple of (signal_index, is_definite, direction, reasoning).

    Raises:
        ValueError: If the line is malformed.
    """
    parts = line.strip().split("|")
    if len(parts) < 4:
        raise ValueError(f"Expected at least 4 pipe-delimited fields, got {len(parts)}")

    signal_index = int(parts[0].strip())
    classification = parts[1].strip().upper()
    direction_str = parts[2].strip().upper()
    reasoning = parts[3].strip()

    is_definite = classification == "DEFINITE"
    direction = _DIRECTION_MAP.get(direction_str) if is_definite else None

    return signal_index, is_definite, direction, reasoning


def parse_response(
    response_text: str,
    group: SignalGroup,
) -> tuple[ConcretisedSignal, ...]:
    """Parse an LLM response into ConcretisedSignal objects.

    Each line of the response corresponds to one signal in the group.
    Malformed or missing lines default to not-definite.

    Args:
        response_text: Raw text response from the LLM.
        group: The signal group that was submitted to the LLM.

    Returns:
        Tuple of ConcretisedSignal objects, one per signal in the group.
    """
    parsed: dict[int, ConcretisedSignal] = {}
    lines = [
        line.strip()
        for line in response_text.strip().splitlines()
        if line.strip() and not line.strip().startswith("#")
    ]

    for line in lines:
        try:
            signal_index, is_definite, direction, reasoning = _parse_single_line(line)
        except (ValueError, IndexError):
            continue

        if signal_index < 0 or signal_index >= len(group.signals):
            continue

        signal = group.signals[signal_index]
        parsed[signal_index] = ConcretisedSignal(
            original_signal=signal,
            ast_context=group.ast_context,
            is_definite=is_definite,
            direction=direction,
            reasoning=reasoning,
        )

    return tuple(
        parsed.get(
            i,
            _make_not_definite(
                signal, group.ast_context, "No valid LLM response for this signal"
            ),
        )
        for i, signal in enumerate(group.signals)
    )
