"""Pure functions for loading and evaluating the signal classifier.

All functions are side-effect free beyond I/O, and return frozen dataclasses.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

from repo_surveyor.training.types import TrainingExample, TrainingLabel
from repo_surveyor.training.signal_classifier import SignalClassifier


@dataclass(frozen=True)
class EvaluationResult:
    """Per-class and aggregate metrics from classifier evaluation.

    Attributes:
        accuracy: Fraction of correctly classified examples.
        per_class_precision: Precision per TrainingLabel value string.
        per_class_recall: Recall per TrainingLabel value string.
        per_class_f1: F1 per TrainingLabel value string.
    """

    accuracy: float
    per_class_precision: dict[str, float]
    per_class_recall: dict[str, float]
    per_class_f1: dict[str, float]


def load_examples(path: Path) -> list[TrainingExample]:
    """Read a JSONL file and return a list of TrainingExample objects.

    Args:
        path: Path to the JSONL file produced by the export pipeline.

    Returns:
        List of parsed TrainingExample instances.
    """
    lines = path.read_text(encoding="utf-8").splitlines()
    return [_parse_example(json.loads(line)) for line in lines if line.strip()]


def _parse_example(record: dict) -> TrainingExample:
    """Parse a single JSON record into a TrainingExample."""
    return TrainingExample(
        id=record["id"],
        language=record["language"],
        integration_type=record["integration_type"],
        label=record["label"],
        code_snippet=record["code_snippet"],
        signal_line_index=record["signal_line_index"],
        signal_line_content=record["signal_line_content"],
        matched_pattern=record["matched_pattern"],
        ast_node_type=record["ast_node_type"],
        framework=record["framework"],
    )


def evaluate(
    classifier: SignalClassifier,
    examples: list[TrainingExample],
) -> EvaluationResult:
    """Evaluate a trained classifier against labelled examples.

    Args:
        classifier: A fitted SignalClassifier.
        examples: Labelled examples to evaluate against.

    Returns:
        EvaluationResult with accuracy and per-class metrics.
    """
    from sklearn.metrics import precision_recall_fscore_support, accuracy_score

    texts = [ex.signal_line_content for ex in examples]
    true_labels = [ex.label for ex in examples]
    pred_labels = [classifier.predict(ex.signal_line_content).value for ex in examples]

    accuracy = float(accuracy_score(true_labels, pred_labels))

    label_values = [label.value for label in TrainingLabel]
    precision_arr, recall_arr, f1_arr, _ = precision_recall_fscore_support(
        true_labels,
        pred_labels,
        labels=label_values,
        zero_division=0,
    )

    per_class_precision = {
        label: float(p) for label, p in zip(label_values, precision_arr)
    }
    per_class_recall = {label: float(r) for label, r in zip(label_values, recall_arr)}
    per_class_f1 = {label: float(f) for label, f in zip(label_values, f1_arr)}

    return EvaluationResult(
        accuracy=accuracy,
        per_class_precision=per_class_precision,
        per_class_recall=per_class_recall,
        per_class_f1=per_class_f1,
    )
