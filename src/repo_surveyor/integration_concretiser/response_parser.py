"""Parse LLM responses into ConcretisedSignal objects."""

import re

from ..integration_detector import IntegrationSignal
from .grouper import SignalGroup
from .types import ASTContext, ConcretisedSignal, IntegrationDirection


class _ResponseFormat:
    """Constants for the pipe-delimited LLM response format."""

    EXPECTED_FIELD_COUNT = 4
    DEFINITE_LABEL = "DEFINITE"
    DEFAULT_REASONING = "No valid LLM response for this signal"
    GROUP_DELIMITER_PATTERN = re.compile(r"---GROUP\s+(\d+)---")
    GROUP_END_PATTERN = re.compile(r"---END GROUP\s+(\d+)---")


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
        direction=IntegrationDirection.UNKNOWN,
        reasoning=reasoning,
    )


def _parse_single_line(line: str) -> tuple[int, bool, IntegrationDirection, str]:
    """Parse a single response line.

    Returns:
        Tuple of (signal_index, is_definite, direction, reasoning).

    Raises:
        ValueError: If the line is malformed.
    """
    parts = line.strip().split("|")
    if len(parts) < _ResponseFormat.EXPECTED_FIELD_COUNT:
        raise ValueError(
            f"Expected at least {_ResponseFormat.EXPECTED_FIELD_COUNT} "
            f"pipe-delimited fields, got {len(parts)}"
        )

    signal_index = int(parts[0].strip())
    classification = parts[1].strip().upper()
    direction_str = parts[2].strip().upper()
    reasoning = parts[3].strip()

    is_definite = classification == _ResponseFormat.DEFINITE_LABEL
    direction = (
        _DIRECTION_MAP.get(direction_str, IntegrationDirection.UNKNOWN)
        if is_definite
        else IntegrationDirection.UNKNOWN
    )

    return signal_index, is_definite, direction, reasoning


def _is_valid_line(line: str) -> bool:
    """Return True if the line is non-empty and not a delimiter or comment."""
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        return False
    if _ResponseFormat.GROUP_DELIMITER_PATTERN.match(stripped):
        return False
    if _ResponseFormat.GROUP_END_PATTERN.match(stripped):
        return False
    return True


def _try_parse_line(
    line: str, group: SignalGroup
) -> tuple[int, ConcretisedSignal] | None:
    """Attempt to parse a line into an indexed ConcretisedSignal.

    Returns None if the line is malformed or the index is out of range.
    """
    try:
        signal_index, is_definite, direction, reasoning = _parse_single_line(line)
    except (ValueError, IndexError):
        return None

    if signal_index < 0 or signal_index >= len(group.signals):
        return None

    return signal_index, ConcretisedSignal(
        original_signal=group.signals[signal_index],
        ast_context=group.ast_context,
        is_definite=is_definite,
        direction=direction,
        reasoning=reasoning,
    )


def _parse_lines_for_group(
    lines: list[str],
    group: SignalGroup,
) -> tuple[ConcretisedSignal, ...]:
    """Parse a list of response lines into ConcretisedSignals for a group."""
    valid_lines = [line.strip() for line in lines if _is_valid_line(line)]

    parsed: dict[int, ConcretisedSignal] = dict(
        result
        for line in valid_lines
        for result in [_try_parse_line(line, group)]
        if result is not None
    )

    return tuple(
        parsed.get(
            i,
            _make_not_definite(
                signal, group.ast_context, _ResponseFormat.DEFAULT_REASONING
            ),
        )
        for i, signal in enumerate(group.signals)
    )


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
    return _parse_lines_for_group(response_text.strip().splitlines(), group)


def _split_batched_response(response_text: str) -> dict[int, list[str]]:
    """Split a batched LLM response into per-group line lists.

    Expects response delimited by ---GROUP N--- / ---END GROUP N---.
    Lines not inside any group delimiter are discarded.

    Returns:
        Dict mapping group index to list of response lines.
    """
    sections: dict[int, list[str]] = {}
    current_group: int | None = None

    for line in response_text.strip().splitlines():
        stripped = line.strip()
        start_match = _ResponseFormat.GROUP_DELIMITER_PATTERN.match(stripped)
        if start_match:
            current_group = int(start_match.group(1))
            sections.setdefault(current_group, [])
            continue

        end_match = _ResponseFormat.GROUP_END_PATTERN.match(stripped)
        if end_match:
            current_group = None
            continue

        if current_group is not None:
            sections[current_group].append(line)

    return sections


def parse_batched_response(
    response_text: str,
    groups: list[SignalGroup],
) -> tuple[tuple[ConcretisedSignal, ...], ...]:
    """Parse a batched LLM response into per-group ConcretisedSignal tuples.

    Args:
        response_text: Raw batched response with group delimiters.
        groups: The signal groups in batch order.

    Returns:
        Tuple of tuples, one per group, each containing ConcretisedSignals.
    """
    sections = _split_batched_response(response_text)

    return tuple(
        _parse_lines_for_group(sections.get(i, []), group)
        for i, group in enumerate(groups)
    )
