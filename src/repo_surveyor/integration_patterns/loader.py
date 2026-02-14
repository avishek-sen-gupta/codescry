"""Shared loader for language integration pattern packages."""

import importlib
import pkgutil
from pathlib import Path

from .types import BasePatternSpec


def load_language_patterns(
    package_name: str, package_path: str, base: BasePatternSpec
) -> tuple[dict, dict]:
    """Load base and framework patterns for a language package.

    Extracts the base patterns from the given spec and auto-discovers
    framework modules (any sibling module exporting a ``FRAMEWORK`` attribute)
    via ``pkgutil.iter_modules``.

    Args:
        package_name: The ``__name__`` of the calling package.
        package_path: The ``__file__`` of the calling package's ``__init__``.
        base: The ``BasePatternSpec`` defined in the language's ``base`` module.

    Returns:
        A ``(base_patterns, framework_patterns)`` tuple.
    """
    base_patterns = base.patterns
    framework_patterns: dict = {}

    pkg_dir = str(Path(package_path).parent)
    for _importer, modname, _ispkg in pkgutil.iter_modules([pkg_dir]):
        if modname == "base":
            continue
        module = importlib.import_module(f".{modname}", package=package_name)
        if hasattr(module, "FRAMEWORK"):
            framework_patterns[module.FRAMEWORK.name] = module.FRAMEWORK.patterns

    return base_patterns, framework_patterns
