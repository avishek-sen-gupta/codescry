#!/usr/bin/env python3
"""Train and evaluate the TF-IDF + Logistic Regression signal classifier.

Usage:
    poetry run python scripts/train_signal_classifier.py \\
        --train data/training/train.jsonl \\
        --val   data/training/val.jsonl \\
        --test  data/training/test.jsonl \\
        --output data/training/signal_classifier.joblib
"""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from repo_surveyor.training.classifier_trainer import load_examples, evaluate
from repo_surveyor.training.signal_classifier import SignalClassifier


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Train the integration signal classifier."
    )
    parser.add_argument(
        "--train",
        type=Path,
        required=True,
        help="Path to train.jsonl",
    )
    parser.add_argument(
        "--val",
        type=Path,
        required=True,
        help="Path to val.jsonl",
    )
    parser.add_argument(
        "--test",
        type=Path,
        required=True,
        help="Path to test.jsonl",
    )
    parser.add_argument(
        "--output",
        type=Path,
        required=True,
        help="Path to write the serialised model (.joblib)",
    )
    return parser


def _print_metrics(split: str, result) -> None:
    print(f"\n{split} accuracy: {result.accuracy:.4f}")
    print(f"{'Label':<22} {'Precision':>10} {'Recall':>10} {'F1':>10}")
    print("-" * 56)
    for label in result.per_class_f1:
        p = result.per_class_precision[label]
        r = result.per_class_recall[label]
        f = result.per_class_f1[label]
        print(f"{label:<22} {p:>10.4f} {r:>10.4f} {f:>10.4f}")


def main() -> None:
    args = _build_parser().parse_args()

    print(f"Loading training data from {args.train} ...")
    train_examples = load_examples(args.train)
    print(f"  {len(train_examples)} train examples")

    print(f"Loading validation data from {args.val} ...")
    val_examples = load_examples(args.val)
    print(f"  {len(val_examples)} val examples")

    print(f"Loading test data from {args.test} ...")
    test_examples = load_examples(args.test)
    print(f"  {len(test_examples)} test examples")

    print("\nTraining classifier ...")
    classifier = SignalClassifier.train(train_examples, val_examples)
    print(f"  Model: {classifier.model_id}")

    val_result = evaluate(classifier, val_examples)
    _print_metrics("Val", val_result)

    test_result = evaluate(classifier, test_examples)
    _print_metrics("Test", test_result)

    classifier.save(args.output)
    print(f"\nModel saved to {args.output}")


if __name__ == "__main__":
    main()
