"""Coverage matrix for training data generation.

Determines which (language, integration_type) pairs have registered
patterns and are therefore valid targets for training example generation.
"""

from dataclasses import dataclass

from repo_surveyor.integration_patterns import (
    get_patterns_for_language,
    LANGUAGE_MODULES,
)
from repo_surveyor.integration_patterns.types import (
    IntegrationType,
    Language,
    PatternKey,
)

from repo_surveyor.training.types import TRAINING_LABELS, TrainingLabel


@dataclass(frozen=True)
class CoverageEntry:
    """A single (language, integration_type) pair with its pattern sources."""

    language: Language
    integration_type: IntegrationType
    pattern_count: int
    sample_patterns: tuple[str, ...]


@dataclass(frozen=True)
class CoverageMatrix:
    """Complete coverage matrix across all languages and integration types."""

    entries: tuple[CoverageEntry, ...]
    total_triples: int
    languages_covered: int
    integration_types_covered: int

    def triples(self) -> list[tuple[str, str, str]]:
        """Return all (language, integration_type, label) triples to generate."""
        return [
            (entry.language.value, entry.integration_type.value, label.value)
            for entry in self.entries
            for label in TRAINING_LABELS
        ]

    def entries_for_language(self, language: Language) -> tuple[CoverageEntry, ...]:
        """Return entries filtered to a specific language."""
        return tuple(e for e in self.entries if e.language == language)

    def patterns_for_entry(self, entry: CoverageEntry) -> tuple[str, ...]:
        """Return the sample patterns for a coverage entry."""
        return entry.sample_patterns

    def to_dict(self) -> dict:
        """Serialise coverage matrix for JSON export."""
        return {
            "total_triples": self.total_triples,
            "languages_covered": self.languages_covered,
            "integration_types_covered": self.integration_types_covered,
            "entries": [
                {
                    "language": e.language.value,
                    "integration_type": e.integration_type.value,
                    "pattern_count": e.pattern_count,
                    "sample_patterns": list(e.sample_patterns),
                }
                for e in self.entries
            ],
        }


_MAX_SAMPLE_PATTERNS = 3


def _collect_all_patterns_for_language(
    language: Language,
) -> dict[IntegrationType, list[str]]:
    """Collect all pattern regexes for a language, including all frameworks."""
    lang_module = LANGUAGE_MODULES.get(language)
    if lang_module is None:
        return {}

    all_frameworks = list(lang_module.FRAMEWORK_PATTERNS.keys())
    patterns_by_type = get_patterns_for_language(language, all_frameworks)

    return {
        itype: [regex for regex, _confidence, _source, _direction, _descs in patterns]
        for itype, patterns in patterns_by_type.items()
        if patterns
    }


def build_coverage_matrix(
    languages: list[Language] = [],
    integration_types: list[IntegrationType] = [],
) -> CoverageMatrix:
    """Build the coverage matrix by inspecting registered patterns.

    Args:
        languages: Languages to include (all if empty).
        integration_types: Integration types to include (all if empty).

    Returns:
        CoverageMatrix with all valid (language, integration_type) pairs.
    """
    target_languages = languages or list(Language)
    target_types = integration_types or list(IntegrationType)

    entries: list[CoverageEntry] = []
    languages_seen: set[Language] = set()
    types_seen: set[IntegrationType] = set()

    for language in target_languages:
        patterns_by_type = _collect_all_patterns_for_language(language)

        for itype in target_types:
            regexes = patterns_by_type.get(itype, [])
            if not regexes:
                continue

            entries.append(
                CoverageEntry(
                    language=language,
                    integration_type=itype,
                    pattern_count=len(regexes),
                    sample_patterns=tuple(regexes[:_MAX_SAMPLE_PATTERNS]),
                )
            )
            languages_seen.add(language)
            types_seen.add(itype)

    total_triples = len(entries) * len(TRAINING_LABELS)

    return CoverageMatrix(
        entries=tuple(entries),
        total_triples=total_triples,
        languages_covered=len(languages_seen),
        integration_types_covered=len(types_seen),
    )
