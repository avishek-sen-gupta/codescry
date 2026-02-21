"""Tests for training data coverage matrix."""

from repo_surveyor.integration_patterns.types import IntegrationType, Language
from repo_surveyor.training.coverage import (
    CoverageEntry,
    CoverageMatrix,
    build_coverage_matrix,
)
from repo_surveyor.training.types import TRAINING_LABELS


class TestBuildCoverageMatrix:
    def test_returns_coverage_matrix(self):
        matrix = build_coverage_matrix()
        assert isinstance(matrix, CoverageMatrix)

    def test_has_entries(self):
        matrix = build_coverage_matrix()
        assert len(matrix.entries) > 0

    def test_all_entries_have_patterns(self):
        matrix = build_coverage_matrix()
        for entry in matrix.entries:
            assert entry.pattern_count > 0
            assert len(entry.sample_patterns) > 0

    def test_filter_by_language(self):
        matrix = build_coverage_matrix(languages=[Language.JAVA])
        assert matrix.languages_covered == 1
        assert all(e.language == Language.JAVA for e in matrix.entries)

    def test_filter_by_integration_type(self):
        matrix = build_coverage_matrix(integration_types=[IntegrationType.HTTP_REST])
        assert matrix.integration_types_covered == 1
        assert all(
            e.integration_type == IntegrationType.HTTP_REST for e in matrix.entries
        )

    def test_filter_by_both(self):
        matrix = build_coverage_matrix(
            languages=[Language.JAVA],
            integration_types=[IntegrationType.HTTP_REST],
        )
        assert len(matrix.entries) == 1
        assert matrix.entries[0].language == Language.JAVA
        assert matrix.entries[0].integration_type == IntegrationType.HTTP_REST

    def test_total_triples_is_entries_times_labels(self):
        matrix = build_coverage_matrix(languages=[Language.JAVA])
        assert matrix.total_triples == len(matrix.entries) * len(TRAINING_LABELS)

    def test_java_has_http_rest(self):
        matrix = build_coverage_matrix(
            languages=[Language.JAVA],
            integration_types=[IntegrationType.HTTP_REST],
        )
        assert len(matrix.entries) == 1

    def test_sample_patterns_capped(self):
        matrix = build_coverage_matrix(
            languages=[Language.JAVA],
            integration_types=[IntegrationType.HTTP_REST],
        )
        # Java HTTP_REST has many patterns, but sample is capped at 3
        assert len(matrix.entries[0].sample_patterns) <= 3


class TestCoverageMatrixMethods:
    def test_triples_generates_all_label_combos(self):
        matrix = build_coverage_matrix(
            languages=[Language.JAVA],
            integration_types=[IntegrationType.HTTP_REST],
        )
        triples = matrix.triples()
        assert len(triples) == 3
        labels = {t[2] for t in triples}
        assert labels == {"DEFINITE_INWARD", "DEFINITE_OUTWARD", "NOT_DEFINITE"}

    def test_entries_for_language(self):
        matrix = build_coverage_matrix(
            languages=[Language.JAVA, Language.PYTHON],
        )
        java_entries = matrix.entries_for_language(Language.JAVA)
        assert all(e.language == Language.JAVA for e in java_entries)
        assert len(java_entries) > 0

    def test_to_dict(self):
        matrix = build_coverage_matrix(
            languages=[Language.JAVA],
            integration_types=[IntegrationType.HTTP_REST],
        )
        d = matrix.to_dict()
        assert "total_triples" in d
        assert "entries" in d
        assert len(d["entries"]) == 1
        assert d["entries"][0]["language"] == "Java"
