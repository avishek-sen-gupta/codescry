"""COBOL integration patterns."""

from repo_surveyor.integration_patterns.cobol.base import BASE
from repo_surveyor.integration_patterns.loader import load_language_patterns

BASE_PATTERNS, FRAMEWORK_PATTERNS, FRAMEWORK_IMPORT_PATTERNS = load_language_patterns(
    __name__, __file__, BASE
)
