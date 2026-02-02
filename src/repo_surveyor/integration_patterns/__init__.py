"""Integration patterns for detecting system integration points.

Patterns are organized by language to support language-specific conventions.
"""

from .types import Confidence, IntegrationType
from . import common
from . import java
from . import rust
from . import python
from . import typescript
from . import javascript
from . import go
from . import csharp
from . import cobol

# File extension to language mapping
EXTENSION_TO_LANGUAGE = {
    ".java": "Java",
    ".rs": "Rust",
    ".py": "Python",
    ".ts": "TypeScript",
    ".tsx": "TypeScript",
    ".js": "JavaScript",
    ".jsx": "JavaScript",
    ".go": "Go",
    ".cs": "C#",
    ".kt": "Kotlin",
    ".scala": "Scala",
    ".rb": "Ruby",
    ".php": "PHP",
    ".cbl": "COBOL",
    ".cob": "COBOL",
    ".cpy": "COBOL",
}

# Map language names to their pattern modules
LANGUAGE_MODULES = {
    "Java": java,
    "Rust": rust,
    "Python": python,
    "TypeScript": typescript,
    "JavaScript": javascript,
    "Go": go,
    "C#": csharp,
    "COBOL": cobol,
}


def get_patterns_for_language(
    language: str,
) -> dict[IntegrationType, list[tuple[str, Confidence]]]:
    """Get integration patterns for a specific language.

    Combines common patterns with language-specific patterns.

    Args:
        language: The programming language.

    Returns:
        Dict mapping IntegrationType to list of (pattern, confidence) tuples.
    """
    result: dict[IntegrationType, list[tuple[str, Confidence]]] = {}

    for integration_type in IntegrationType:
        patterns: list[tuple[str, Confidence]] = []

        # Add common patterns
        common_type_patterns = common.PATTERNS.get(integration_type, {})
        patterns.extend(common_type_patterns.get("patterns", []))

        # Add language-specific patterns
        lang_module = LANGUAGE_MODULES.get(language)
        if lang_module is not None:
            lang_type_patterns = lang_module.PATTERNS.get(integration_type, {})
            patterns.extend(lang_type_patterns.get("patterns", []))

        result[integration_type] = patterns

    return result


def get_directory_patterns() -> dict[IntegrationType, list[str]]:
    """Get directory patterns for all integration types.

    Returns:
        Dict mapping IntegrationType to list of directory patterns.
    """
    return {
        integration_type: patterns.get("directory_patterns", [])
        for integration_type, patterns in common.PATTERNS.items()
    }


__all__ = [
    "Confidence",
    "IntegrationType",
    "EXTENSION_TO_LANGUAGE",
    "LANGUAGE_MODULES",
    "get_patterns_for_language",
    "get_directory_patterns",
    "common",
    "java",
    "rust",
    "python",
    "typescript",
    "javascript",
    "go",
    "csharp",
    "cobol",
]
