"""Export validated training examples to JSONL with stratified splits.

Produces train/val/test splits (80/10/10) ensuring each
(language, integration_type, label) triple is proportionally represented.
"""

import json
import logging
import random
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

from .types import TrainingExample

logger = logging.getLogger(__name__)


class _SplitRatios:
    """Default train/val/test split ratios."""

    TRAIN = 0.8
    VAL = 0.1
    TEST = 0.1


class _FileNames:
    """Output file names."""

    ALL = "examples.jsonl"
    TRAIN = "train.jsonl"
    VAL = "val.jsonl"
    TEST = "test.jsonl"
    COVERAGE = "coverage.json"


@dataclass(frozen=True)
class ExportResult:
    """Result of exporting training data."""

    total_examples: int
    train_count: int
    val_count: int
    test_count: int
    output_dir: str


def _stratification_key(example: TrainingExample) -> str:
    """Produce the stratification key for an example."""
    return f"{example.language}__{example.integration_type}__{example.label}"


def _stratified_split(
    examples: list[TrainingExample],
    seed: int = 42,
) -> tuple[list[TrainingExample], list[TrainingExample], list[TrainingExample]]:
    """Split examples into train/val/test with stratification by triple.

    Each (language, integration_type, label) group is independently shuffled
    and split so that the proportions hold within each stratum.

    Args:
        examples: All validated examples.
        seed: Random seed for reproducibility.

    Returns:
        Tuple of (train, val, test) example lists.
    """
    rng = random.Random(seed)

    groups: dict[str, list[TrainingExample]] = defaultdict(list)
    for ex in examples:
        groups[_stratification_key(ex)].append(ex)

    train: list[TrainingExample] = []
    val: list[TrainingExample] = []
    test: list[TrainingExample] = []

    for _key, group_examples in sorted(groups.items()):
        rng.shuffle(group_examples)
        n = len(group_examples)
        n_val = max(1, round(n * _SplitRatios.VAL)) if n >= 3 else 0
        n_test = max(1, round(n * _SplitRatios.TEST)) if n >= 3 else 0
        n_train = n - n_val - n_test

        train.extend(group_examples[:n_train])
        val.extend(group_examples[n_train : n_train + n_val])
        test.extend(group_examples[n_train + n_val :])

    return train, val, test


def _write_jsonl(examples: list[TrainingExample], path: Path) -> None:
    """Write examples to a JSONL file."""
    with path.open("w", encoding="utf-8") as f:
        for example in examples:
            f.write(json.dumps(example.to_dict(), ensure_ascii=False) + "\n")
    logger.info("Wrote %d examples to %s", len(examples), path)


def export_training_data(
    examples: list[TrainingExample],
    output_dir: Path,
    seed: int = 42,
) -> ExportResult:
    """Export validated examples to JSONL files with stratified splits.

    Args:
        examples: All validated training examples.
        output_dir: Directory to write output files.
        seed: Random seed for split reproducibility.

    Returns:
        ExportResult with counts and output path.
    """
    output_dir.mkdir(parents=True, exist_ok=True)

    _write_jsonl(examples, output_dir / _FileNames.ALL)

    train, val, test = _stratified_split(examples, seed=seed)

    _write_jsonl(train, output_dir / _FileNames.TRAIN)
    _write_jsonl(val, output_dir / _FileNames.VAL)
    _write_jsonl(test, output_dir / _FileNames.TEST)

    logger.info(
        "Export complete: %d total, %d train, %d val, %d test",
        len(examples),
        len(train),
        len(val),
        len(test),
    )

    return ExportResult(
        total_examples=len(examples),
        train_count=len(train),
        val_count=len(val),
        test_count=len(test),
        output_dir=str(output_dir),
    )
