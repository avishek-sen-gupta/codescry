"""Prompt construction and chunking for the ML classifier."""

from .types import MLIntegrationType

_CATEGORIES = ", ".join(t.value for t in MLIntegrationType)

SYSTEM_PROMPT = f"""You are a code integration classifier. For each line of source code, classify it into exactly one integration category.

Categories: {_CATEGORIES}

Hints for COBOL / mainframe code:
- IDMS DML verbs are database operations: BIND RUN-UNIT (database_connection), READY (database_connection), OBTAIN/FIND (database_query), STORE (database_query), MODIFY (database_query), ERASE (database_query), FINISH (database_connection)
- EXEC SQL is database_query; EXEC SQL INCLUDE SQLCA is database_connection
- EXEC CICS SEND / EXEC CICS RECEIVE are http_server (terminal I/O)
- CALL 'MQCONN' / CALL 'MQOPEN' are message_publish; CALL 'MQPUT' is message_publish; CALL 'MQGET' is message_subscribe; CALL 'MQCLOSE' is message_publish
- CALL 'CBLTDLI' is database_query (IMS DB)

Rules:
- Output one line per input line, in the format: LINE_NUMBER|CATEGORY
- CATEGORY must be one of the listed categories exactly
- If a line does not represent any integration, output LINE_NUMBER|none
- Do not output any explanation, only LINE_NUMBER|CATEGORY lines
- Preserve the original LINE_NUMBER from the input"""

_CHUNK_SIZE = 80


def build_user_prompt(numbered_lines: list[tuple[int, str]]) -> str:
    """Build the user prompt from numbered source lines.

    Args:
        numbered_lines: List of (line_number, line_text) tuples.

    Returns:
        Formatted user prompt string.
    """
    lines = [f"{num}|{text}" for num, text in numbered_lines]
    return "\n".join(lines)


def chunk_lines(
    numbered_lines: list[tuple[int, str]],
    chunk_size: int = _CHUNK_SIZE,
) -> list[list[tuple[int, str]]]:
    """Split numbered lines into chunks for batched model calls.

    Args:
        numbered_lines: List of (line_number, line_text) tuples.
        chunk_size: Maximum lines per chunk.

    Returns:
        List of chunks, each a list of (line_number, line_text) tuples.
    """
    return [
        numbered_lines[i : i + chunk_size]
        for i in range(0, len(numbered_lines), chunk_size)
    ]
