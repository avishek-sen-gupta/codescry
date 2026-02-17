"""LLM-based classification of tree-sitter grammar node types into ControlFlowRole values."""

import json
import subprocess
import tempfile
from pathlib import Path

from repo_surveyor.cfg_constructor.types import ControlFlowRole

_EXTRACT_RULES_SCRIPT = Path(__file__).parents[3] / "js" / "extract_rules.js"

_ROLE_NAMES = ", ".join(role.name for role in ControlFlowRole)

_ROLE_DESCRIPTIONS = """
- SEQUENCE: A block or compound statement that groups child statements executed in order (e.g., block, compound_statement).
- BRANCH: A conditional statement that chooses one of two paths (e.g., if_statement, ternary_expression).
- SWITCH: A multi-way branch based on a value (e.g., switch_statement, match_expression).
- LOOP: A loop with the condition tested before the body (e.g., for_statement, while_statement, for_each_statement, enhanced_for_statement).
- LOOP_POST_CONDITION: A loop with the condition tested after the body (e.g., do_statement, do_while_statement).
- RETURN: A statement that exits the current function (e.g., return_statement, yield_statement).
- BREAK: A statement that exits the enclosing loop or switch (e.g., break_statement).
- CONTINUE: A statement that skips to the next iteration of the enclosing loop (e.g., continue_statement).
- THROW: A statement that raises an exception (e.g., throw_statement, raise_statement).
- TRY: A try/catch/finally construct for exception handling (e.g., try_statement, try_with_resources_statement).
- CALL: A function or method invocation (e.g., method_invocation, call_expression).
- LEAF: Any other statement or expression that does not alter control flow (e.g., assignment, variable_declaration).
""".strip()

SYSTEM_PROMPT = f"""You are a programming-language grammar analyst. You will receive a list of named node types extracted from a tree-sitter grammar for a programming language.

Your task: classify each node type into exactly one control flow role.

Roles:
{_ROLE_DESCRIPTIONS}

Rules:
- Output one line per classified node type, in the format: NODE_TYPE|ROLE
- ROLE must be one of: {_ROLE_NAMES}
- Only classify node types that represent statements, expressions, or blocks.
- Skip punctuation, keywords, operators, delimiters, and purely structural tokens by omitting them entirely.
- If a node type does not clearly alter control flow, classify it as LEAF.
- Do not output any explanation, only NODE_TYPE|ROLE lines."""

_NODE_TIMEOUT_SECONDS = 30


def extract_node_types(grammar_js: str) -> list[str]:
    """Extract rule names from a tree-sitter grammar.js by executing it with a stub.

    Runs the grammar.js file under Node.js with a stub ``grammar()`` function
    that intercepts the rules object and outputs its keys as JSON.

    Args:
        grammar_js: The full text of a tree-sitter grammar.js file.

    Returns:
        Sorted list of rule name strings.

    Raises:
        FileNotFoundError: If Node.js is not installed.
        RuntimeError: If the Node.js extraction script fails.
    """
    with tempfile.TemporaryDirectory() as tmp_dir:
        grammar_path = Path(tmp_dir) / "grammar.js"
        grammar_path.write_text(grammar_js)
        result = subprocess.run(
            ["node", str(_EXTRACT_RULES_SCRIPT), str(grammar_path)],
            capture_output=True,
            text=True,
            timeout=_NODE_TIMEOUT_SECONDS,
        )
        if result.returncode != 0:
            raise RuntimeError(
                f"extract_rules.js failed (exit {result.returncode}): {result.stderr}"
            )
        return sorted(json.loads(result.stdout))


def build_user_prompt(node_types: list[str]) -> str:
    """Format a list of node type names as a user prompt for the classifier.

    Args:
        node_types: List of tree-sitter node type name strings.

    Returns:
        Formatted user prompt string listing the node type names.
    """
    listing = "\n".join(node_types)
    return (
        "Below are the named node types from a tree-sitter grammar. "
        "Classify each one.\n\n"
        f"{listing}"
    )


_ROLE_LOOKUP: dict[str, ControlFlowRole] = {
    role.name.lower(): role for role in ControlFlowRole
}


def parse_classification_response(text: str) -> dict[str, ControlFlowRole]:
    """Parse NODE_TYPE|ROLE lines from model output into a mapping.

    Malformed lines and unrecognised roles are silently skipped.

    Args:
        text: Raw model response text.

    Returns:
        Mapping from tree-sitter node type name to its ControlFlowRole.
    """
    result: dict[str, ControlFlowRole] = {}
    for line in text.splitlines():
        parts = line.strip().split("|")
        if len(parts) != 2:
            continue
        node_type, role_str = parts[0].strip(), parts[1].strip().lower()
        if not node_type or role_str not in _ROLE_LOOKUP:
            continue
        result[node_type] = _ROLE_LOOKUP[role_str]
    return result
