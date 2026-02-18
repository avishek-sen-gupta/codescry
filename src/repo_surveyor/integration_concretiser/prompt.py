"""Prompt construction for integration signal concretisation."""

from .grouper import SignalGroup

_SYSTEM_PROMPT = """\
You are a code analyst that classifies integration signals in source code.

For each detected integration signal in a code block, determine:
1. Whether it is a DEFINITE integration point (actual runtime integration) or NOT (import, type annotation, configuration declaration, unused reference).
2. If definite, whether its direction is INWARD (receiving: HTTP handler, message consumer, event listener, server endpoint) or OUTWARD (sending: HTTP client call, message publish, database write/read, external API call).

Rules:
- Import statements are NOT definite integration points.
- Type annotations and interface declarations alone are NOT definite.
- Configuration declarations (e.g., connection strings, URLs in config) are NOT definite.
- Annotations on methods (e.g., @GetMapping, @PostMapping) ARE definite — they define active endpoints.
- Method calls to external systems (e.g., requests.get, db.execute, queue.publish) ARE definite.
- Framework handler registrations (e.g., app.get('/path', handler)) ARE definite.

Output format — one line per signal, using the signal's index number:
SIGNAL_INDEX|DEFINITE|DIRECTION|BRIEF_REASON
or
SIGNAL_INDEX|NOT_DEFINITE||BRIEF_REASON

Where:
- SIGNAL_INDEX is the 0-based index from the signal list
- DEFINITE or NOT_DEFINITE indicates classification
- DIRECTION is INWARD or OUTWARD (empty if NOT_DEFINITE)
- BRIEF_REASON is a short explanation (max 20 words)

Output ONLY the formatted lines, no other text."""

_USER_PROMPT_TEMPLATE = """\
AST node type: {node_type}
File: {file_path}

Code block:
```
{node_text}
```

Detected integration signals:
{signal_list}"""

_SIGNAL_LINE_TEMPLATE = "{index}. Line {line_number}: [{integration_type}] matched pattern: {matched_pattern}"


def build_system_prompt() -> str:
    """Return the system prompt for integration signal concretisation."""
    return _SYSTEM_PROMPT


def build_user_prompt(group: SignalGroup) -> str:
    """Build the user prompt for a signal group.

    Args:
        group: A group of co-located signals sharing an AST context.

    Returns:
        Formatted user prompt including the code block and signal list.
    """
    signal_lines = [
        _SIGNAL_LINE_TEMPLATE.format(
            index=i,
            line_number=signal.match.line_number,
            integration_type=signal.integration_type.value,
            matched_pattern=signal.matched_pattern,
        )
        for i, signal in enumerate(group.signals)
    ]

    return _USER_PROMPT_TEMPLATE.format(
        node_type=group.ast_context.node_type,
        file_path=group.file_path,
        node_text=group.ast_context.node_text,
        signal_list="\n".join(signal_lines),
    )
