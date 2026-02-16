"""C# integration patterns."""

from .base import BASE
from ..loader import load_language_patterns

BASE_PATTERNS, FRAMEWORK_PATTERNS, FRAMEWORK_IMPORT_PATTERNS = load_language_patterns(
    __name__, __file__, BASE
)
