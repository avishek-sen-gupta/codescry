"""TypeScript integration patterns."""

import importlib
import pkgutil
from pathlib import Path

from .base import BASE_PATTERNS

FRAMEWORK_PATTERNS: dict = {}

_pkg_path = str(Path(__file__).parent)
for _importer, _modname, _ispkg in pkgutil.iter_modules([_pkg_path]):
    if _modname == "base":
        continue
    _module = importlib.import_module(f".{_modname}", package=__name__)
    if hasattr(_module, "NAME") and hasattr(_module, "PATTERNS"):
        FRAMEWORK_PATTERNS[_module.NAME] = _module.PATTERNS
