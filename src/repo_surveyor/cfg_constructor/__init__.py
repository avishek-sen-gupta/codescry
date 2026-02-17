"""CFG constructor module for language-independent control flow graph construction."""

from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec, load_cfg_roles
from repo_surveyor.cfg_constructor.types import ControlFlowRole, LanguageCFGSpec

__all__ = [
    "ControlFlowRole",
    "LanguageCFGSpec",
    "get_cfg_spec",
    "load_cfg_roles",
]
