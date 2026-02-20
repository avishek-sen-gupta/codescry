"""Tests for SignalClassifier and classifier_trainer utilities."""

import json
from pathlib import Path

import pytest

from repo_surveyor.training.types import TrainingExample, TrainingLabel
from repo_surveyor.training.signal_classifier import (
    NullSignalClassifier,
    SignalClassifier,
)
from repo_surveyor.training.classifier_trainer import (
    EvaluationResult,
    load_examples,
    evaluate,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_example(label: str, content: str, seq: int = 0) -> TrainingExample:
    return TrainingExample(
        id=f"Java__http_rest__{label}__spring__{seq}",
        language="Java",
        integration_type="http_rest",
        label=label,
        code_snippet=f"// example {seq}\n{content}",
        signal_line_index=1,
        signal_line_content=content,
        matched_pattern="test_pattern",
        ast_node_type="method_declaration",
        framework="spring",
    )


def _synthetic_examples() -> list[TrainingExample]:
    """Return a small but diverse synthetic dataset (3 classes, 5 per class)."""
    inward = [
        _make_example(
            "DEFINITE_INWARD",
            f'@GetMapping("/api/endpoint/{i}")',
            i,
        )
        for i in range(5)
    ]
    outward = [
        _make_example(
            "DEFINITE_OUTWARD",
            f'restTemplate.postForObject("http://svc/{i}", body, String.class)',
            i,
        )
        for i in range(5)
    ]
    not_def = [
        _make_example(
            "NOT_DEFINITE",
            f"String name = getUser{i}();",
            i,
        )
        for i in range(5)
    ]
    return inward + outward + not_def


# ---------------------------------------------------------------------------
# TestSignalClassifierTraining
# ---------------------------------------------------------------------------


class TestSignalClassifierTraining:
    def test_train_returns_signal_classifier(self):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        assert isinstance(classifier, SignalClassifier)

    def test_predict_returns_training_label(self):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        result = classifier.predict('@GetMapping("/api/users")')
        assert isinstance(result, TrainingLabel)

    def test_predict_proba_covers_all_labels(self):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        proba = classifier.predict_proba(
            "restTemplate.postForObject(url, body, Resp.class)"
        )
        assert set(proba.keys()) == set(TrainingLabel)

    def test_predict_proba_sums_to_one(self):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        proba = classifier.predict_proba('@PostMapping("/submit")')
        total = sum(proba.values())
        assert abs(total - 1.0) < 1e-6

    def test_model_id_is_non_empty_string(self):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        assert isinstance(classifier.model_id, str)
        assert len(classifier.model_id) > 0


# ---------------------------------------------------------------------------
# TestSignalClassifierSerialisation
# ---------------------------------------------------------------------------


class TestSignalClassifierSerialisation:
    def test_save_creates_file(self, tmp_path):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        model_path = tmp_path / "model.joblib"
        classifier.save(model_path)
        assert model_path.exists()

    def test_load_round_trip_identical_predictions(self, tmp_path):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        model_path = tmp_path / "model.joblib"
        classifier.save(model_path)

        loaded = SignalClassifier.load(model_path)
        test_lines = [
            '@GetMapping("/health")',
            "restTemplate.postForObject(url, body, String.class)",
            "String name = getName();",
        ]
        for line in test_lines:
            assert classifier.predict(line) == loaded.predict(line)

    def test_load_round_trip_identical_probabilities(self, tmp_path):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        model_path = tmp_path / "model.joblib"
        classifier.save(model_path)

        loaded = SignalClassifier.load(model_path)
        line = '@PutMapping("/update")'
        orig = classifier.predict_proba(line)
        restored = loaded.predict_proba(line)
        for label in TrainingLabel:
            assert abs(orig[label] - restored[label]) < 1e-9


# ---------------------------------------------------------------------------
# TestEvaluationResult
# ---------------------------------------------------------------------------


class TestEvaluationResult:
    def test_construct_evaluation_result(self):
        result = EvaluationResult(
            accuracy=0.85,
            per_class_precision={
                "DEFINITE_INWARD": 0.9,
                "DEFINITE_OUTWARD": 0.8,
                "NOT_DEFINITE": 0.85,
            },
            per_class_recall={
                "DEFINITE_INWARD": 0.88,
                "DEFINITE_OUTWARD": 0.79,
                "NOT_DEFINITE": 0.82,
            },
            per_class_f1={
                "DEFINITE_INWARD": 0.89,
                "DEFINITE_OUTWARD": 0.795,
                "NOT_DEFINITE": 0.835,
            },
        )
        assert result.accuracy == pytest.approx(0.85)
        assert result.per_class_f1["DEFINITE_INWARD"] == pytest.approx(0.89)

    def test_evaluation_result_is_frozen(self):
        result = EvaluationResult(
            accuracy=0.9,
            per_class_precision={},
            per_class_recall={},
            per_class_f1={},
        )
        with pytest.raises(Exception):
            result.accuracy = 0.5  # type: ignore[misc]

    def test_evaluate_returns_evaluation_result(self):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        result = evaluate(classifier, examples)
        assert isinstance(result, EvaluationResult)
        assert 0.0 <= result.accuracy <= 1.0

    def test_evaluate_per_class_keys_match_labels(self):
        examples = _synthetic_examples()
        classifier = SignalClassifier.train(examples, examples)
        result = evaluate(classifier, examples)
        expected_keys = {label.value for label in TrainingLabel}
        assert set(result.per_class_f1.keys()) == expected_keys
        assert set(result.per_class_precision.keys()) == expected_keys
        assert set(result.per_class_recall.keys()) == expected_keys


# ---------------------------------------------------------------------------
# TestLoadExamples
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# TestNullSignalClassifier
# ---------------------------------------------------------------------------


class TestNullSignalClassifier:
    def test_is_signal_classifier(self):
        assert isinstance(NullSignalClassifier(), SignalClassifier)

    def test_predict_always_returns_not_definite(self):
        null = NullSignalClassifier()
        for line in [
            '@GetMapping("/users")',
            "restTemplate.post(url)",
            "String x = y;",
        ]:
            assert null.predict(line) == TrainingLabel.NOT_DEFINITE

    def test_predict_proba_covers_all_labels(self):
        null = NullSignalClassifier()
        proba = null.predict_proba("anything")
        assert set(proba.keys()) == set(TrainingLabel)

    def test_predict_proba_all_zero(self):
        null = NullSignalClassifier()
        proba = null.predict_proba("anything")
        assert all(v == 0.0 for v in proba.values())

    def test_model_id_is_non_empty_string(self):
        null = NullSignalClassifier()
        assert isinstance(null.model_id, str)
        assert len(null.model_id) > 0


# ---------------------------------------------------------------------------
# TestLoadExamples
# ---------------------------------------------------------------------------


class TestLoadExamples:
    def test_load_examples_returns_list(self, tmp_path):
        jsonl_path = tmp_path / "test.jsonl"
        examples = _synthetic_examples()[:3]
        lines = [json.dumps(ex.to_dict()) for ex in examples]
        jsonl_path.write_text("\n".join(lines) + "\n", encoding="utf-8")

        loaded = load_examples(jsonl_path)
        assert len(loaded) == 3

    def test_load_examples_returns_training_examples(self, tmp_path):
        jsonl_path = tmp_path / "test.jsonl"
        examples = _synthetic_examples()[:2]
        lines = [json.dumps(ex.to_dict()) for ex in examples]
        jsonl_path.write_text("\n".join(lines), encoding="utf-8")

        loaded = load_examples(jsonl_path)
        assert all(isinstance(ex, TrainingExample) for ex in loaded)

    def test_load_examples_preserves_fields(self, tmp_path):
        jsonl_path = tmp_path / "test.jsonl"
        original = _make_example("DEFINITE_INWARD", '@GetMapping("/api/test")', 99)
        jsonl_path.write_text(json.dumps(original.to_dict()) + "\n", encoding="utf-8")

        loaded = load_examples(jsonl_path)
        assert len(loaded) == 1
        ex = loaded[0]
        assert ex.id == original.id
        assert ex.label == original.label
        assert ex.signal_line_content == original.signal_line_content
        assert ex.language == original.language
        assert ex.integration_type == original.integration_type

    def test_load_examples_skips_blank_lines(self, tmp_path):
        jsonl_path = tmp_path / "test.jsonl"
        examples = _synthetic_examples()[:2]
        lines = [json.dumps(ex.to_dict()) for ex in examples]
        jsonl_path.write_text("\n".join(lines) + "\n\n", encoding="utf-8")

        loaded = load_examples(jsonl_path)
        assert len(loaded) == 2
