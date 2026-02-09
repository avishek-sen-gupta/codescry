"""Parse model responses into ClassifiedLine instances."""

from .types import ClassifiedLine, MLIntegrationType

_LABEL_LOOKUP: dict[str, MLIntegrationType] = {t.value: t for t in MLIntegrationType}


def _parse_label(raw_label: str) -> MLIntegrationType:
    """Parse a raw label string into an MLIntegrationType, defaulting to NONE."""
    return _LABEL_LOOKUP.get(raw_label.strip().lower(), MLIntegrationType.NONE)


def parse_classification_response(
    raw_response: str,
    original_lines: dict[int, str],
) -> list[ClassifiedLine]:
    """Parse the model's raw text output into ClassifiedLine instances.

    Args:
        raw_response: Raw model output text with LINE_NUMBER|CATEGORY lines.
        original_lines: Mapping of line_number to original line content.

    Returns:
        List of ClassifiedLine instances for successfully parsed lines.
    """
    results: list[ClassifiedLine] = []

    for response_line in raw_response.strip().splitlines():
        response_line = response_line.strip()
        if not response_line:
            continue

        parts = response_line.split("|", maxsplit=1)
        if len(parts) != 2:
            continue

        raw_num, raw_label = parts

        try:
            line_number = int(raw_num.strip())
        except ValueError:
            continue

        if line_number not in original_lines:
            continue

        integration_type = _parse_label(raw_label)
        results.append(
            ClassifiedLine(
                line_number=line_number,
                line_content=original_lines[line_number],
                integration_type=integration_type,
                raw_model_label=raw_label.strip(),
            )
        )

    # Fill in NONE for any lines the model didn't respond about
    responded_line_numbers = {r.line_number for r in results}
    for line_number, content in sorted(original_lines.items()):
        if line_number not in responded_line_numbers:
            results.append(
                ClassifiedLine(
                    line_number=line_number,
                    line_content=content,
                    integration_type=MLIntegrationType.NONE,
                    raw_model_label="",
                )
            )

    return sorted(results, key=lambda c: c.line_number)
