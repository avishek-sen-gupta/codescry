"""Tests for EvidenceEvaluator and score adjustment logic."""

import json
from pathlib import Path

import pytest

from repo_surveyor.integration_concretiser.checklist_registry import (
    ChecklistRegistry,
)
from repo_surveyor.integration_concretiser.evidence_evaluator import (
    EvidenceEvaluator,
    _clamp,
    _evaluate_checklist,
)
from repo_surveyor.integration_concretiser.evidence_predicates import (
    ChecklistEntry,
    PredicateName,
)
from repo_surveyor.integration_concretiser.predicate_context_builder import (
    PredicateContext,
)
from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import (
    Confidence,
    IntegrationType,
    Language,
    SignalDirection,
)


def _make_ctx(
    file_path: str = "src/main/App.java",
    line_content: str = "conn.open()",
    enclosing_node_text: str = "",
    ancestor_node_types: tuple[str, ...] = (),
) -> PredicateContext:
    return PredicateContext(
        file_path=file_path,
        line_number=10,
        line_content=line_content,
        language=Language.JAVA,
        source_lines=(line_content,),
        enclosing_node_text=(
            enclosing_node_text if enclosing_node_text else line_content
        ),
        enclosing_node_type="method_declaration",
        ancestor_node_types=ancestor_node_types,
    )


def _make_signal(
    file_path: str = "src/main/App.java",
    source: str = "test",
    matched_pattern: str = "test_pattern",
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=10,
            line_content="conn.open()",
            language=Language.JAVA,
        ),
        integration_type=IntegrationType.DATABASE,
        confidence=Confidence.HIGH,
        matched_pattern=matched_pattern,
        entity_type=EntityType.FILE_CONTENT,
        source=source,
    )


# ---------------------------------------------------------------------------
# Clamp
# ---------------------------------------------------------------------------


class TestClamp:
    def test_within_range(self):
        assert _clamp(0.5, 0.0, 1.0) == 0.5

    def test_below_floor(self):
        assert _clamp(-0.2, 0.0, 1.0) == 0.0

    def test_above_ceiling(self):
        assert _clamp(1.3, 0.0, 1.0) == 1.0

    def test_exact_bounds(self):
        assert _clamp(0.0, 0.0, 1.0) == 0.0
        assert _clamp(1.0, 0.0, 1.0) == 1.0


# ---------------------------------------------------------------------------
# Direct checklist evaluation
# ---------------------------------------------------------------------------


class TestEvaluateChecklist:
    def test_empty_checklist_returns_unchanged_score(self):
        ctx = _make_ctx()
        verdict = _evaluate_checklist((), ctx, 0.65)
        assert verdict.original_score == 0.65
        assert verdict.adjusted_score == 0.65
        assert verdict.score_adjustment == 0.0
        assert verdict.predicate_results == ()

    def test_matched_negative_predicate_reduces_score(self):
        ctx = _make_ctx(ancestor_node_types=("identifier", "string_literal"))
        checklist = (
            ChecklistEntry(
                predicate=PredicateName.IN_STRING_CONTEXT,
                weight=-0.3,
                pattern_arg="",
            ),
        )
        verdict = _evaluate_checklist(checklist, ctx, 0.65)
        assert verdict.original_score == 0.65
        assert verdict.score_adjustment == pytest.approx(-0.3)
        assert verdict.adjusted_score == pytest.approx(0.35)
        assert verdict.predicate_results[0].matched is True

    def test_unmatched_predicate_does_not_affect_score(self):
        ctx = _make_ctx(ancestor_node_types=("identifier", "call_expression"))
        checklist = (
            ChecklistEntry(
                predicate=PredicateName.IN_STRING_CONTEXT,
                weight=-0.3,
                pattern_arg="",
            ),
        )
        verdict = _evaluate_checklist(checklist, ctx, 0.65)
        assert verdict.adjusted_score == 0.65
        assert verdict.score_adjustment == 0.0
        assert verdict.predicate_results[0].matched is False

    def test_multiple_predicates_accumulate(self):
        ctx = _make_ctx(
            file_path="tests/test_db.py",
            ancestor_node_types=("identifier", "string_literal"),
        )
        checklist = (
            ChecklistEntry(
                predicate=PredicateName.IN_STRING_CONTEXT,
                weight=-0.3,
                pattern_arg="",
            ),
            ChecklistEntry(
                predicate=PredicateName.IN_TEST_FILE,
                weight=-0.15,
                pattern_arg="",
            ),
        )
        verdict = _evaluate_checklist(checklist, ctx, 0.70)
        assert verdict.score_adjustment == pytest.approx(-0.45)
        assert verdict.adjusted_score == pytest.approx(0.25)

    def test_score_clamped_to_zero(self):
        ctx = _make_ctx(ancestor_node_types=("identifier", "string_literal"))
        checklist = (
            ChecklistEntry(
                predicate=PredicateName.IN_STRING_CONTEXT,
                weight=-0.9,
                pattern_arg="",
            ),
        )
        verdict = _evaluate_checklist(checklist, ctx, 0.3)
        assert verdict.adjusted_score == 0.0

    def test_score_clamped_to_one(self):
        ctx = _make_ctx()
        checklist = (
            ChecklistEntry(
                predicate=PredicateName.LINE_MATCHES,
                weight=0.5,
                pattern_arg=r"conn\.",
            ),
        )
        verdict = _evaluate_checklist(checklist, ctx, 0.8)
        assert verdict.adjusted_score == 1.0


# ---------------------------------------------------------------------------
# EvidenceEvaluator with registry
# ---------------------------------------------------------------------------


class TestEvidenceEvaluator:
    def test_evaluate_uses_default_checklist(self, tmp_path: Path):
        checklist_file = tmp_path / "checklist.json"
        checklist_file.write_text(
            json.dumps(
                {
                    "version": "1.0",
                    "default_predicates": [
                        {
                            "predicate": "IN_STRING_CONTEXT",
                            "weight": -0.3,
                            "pattern_arg": "",
                        }
                    ],
                    "pattern_overrides": {},
                }
            ),
            encoding="utf-8",
        )
        registry = ChecklistRegistry(checklist_file)
        evaluator = EvidenceEvaluator(registry)

        signal = _make_signal()
        ctx = _make_ctx(ancestor_node_types=("identifier", "string_literal"))
        verdict = evaluator.evaluate(signal, ctx, raw_score=0.65)

        assert verdict.adjusted_score == pytest.approx(0.35)

    def test_batch_evaluate(self, tmp_path: Path):
        checklist_file = tmp_path / "checklist.json"
        checklist_file.write_text(
            json.dumps(
                {
                    "version": "1.0",
                    "default_predicates": [
                        {
                            "predicate": "IN_TEST_FILE",
                            "weight": -0.15,
                            "pattern_arg": "",
                        }
                    ],
                    "pattern_overrides": {},
                }
            ),
            encoding="utf-8",
        )
        registry = ChecklistRegistry(checklist_file)
        evaluator = EvidenceEvaluator(registry)

        signals = [
            _make_signal(file_path="tests/test_api.py"),
            _make_signal(file_path="src/main/Api.java"),
        ]
        contexts = [
            _make_ctx(file_path="tests/test_api.py"),
            _make_ctx(file_path="src/main/Api.java"),
        ]
        scores = [0.70, 0.70]

        verdicts = evaluator.batch_evaluate(signals, contexts, scores)

        assert len(verdicts) == 2
        assert verdicts[0].adjusted_score == pytest.approx(0.55)  # test file penalty
        assert verdicts[1].adjusted_score == pytest.approx(0.70)  # no penalty
