"""CFG constructor module for language-independent control flow graph construction."""

from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec, load_cfg_roles
from repo_surveyor.cfg_constructor.grammar_classifier import (
    SYSTEM_PROMPT as GRAMMAR_CLASSIFIER_SYSTEM_PROMPT,
    build_user_prompt as build_grammar_user_prompt,
    extract_node_types,
    parse_classification_response,
)
from repo_surveyor.cfg_constructor.types import ControlFlowRole, LanguageCFGSpec

__all__ = [
    "ControlFlowRole",
    "GRAMMAR_CLASSIFIER_SYSTEM_PROMPT",
    "LanguageCFGSpec",
    "build_grammar_user_prompt",
    "extract_node_types",
    "get_cfg_spec",
    "load_cfg_roles",
    "parse_classification_response",
]
