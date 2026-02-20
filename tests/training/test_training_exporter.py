"""Tests for training data exporter."""

import json

from repo_surveyor.training.types import TrainingExample
from repo_surveyor.training.exporter import export_training_data


def _make_example(language, integration_type, label, seq) -> TrainingExample:
    """Create a minimal valid TrainingExample."""
    return TrainingExample(
        id=f"{language}__{integration_type}__{label}__{seq}",
        language=language,
        integration_type=integration_type,
        label=label,
        code_snippet=f"// example {seq}\ncode_here();",
        signal_line_index=1,
        signal_line_content="code_here();",
        matched_pattern="code_here",
        ast_node_type="method_declaration",
        framework="test",
    )


class TestExportTrainingData:
    def test_creates_output_files(self, tmp_path):
        examples = [
            _make_example("Java", "http_rest", "DEFINITE_INWARD", i) for i in range(10)
        ]
        result = export_training_data(examples, tmp_path)

        assert (tmp_path / "examples.jsonl").exists()
        assert (tmp_path / "train.jsonl").exists()
        assert (tmp_path / "val.jsonl").exists()
        assert (tmp_path / "test.jsonl").exists()

    def test_all_examples_in_main_file(self, tmp_path):
        examples = [
            _make_example("Java", "http_rest", "DEFINITE_INWARD", i) for i in range(5)
        ]
        export_training_data(examples, tmp_path)

        lines = (tmp_path / "examples.jsonl").read_text().strip().split("\n")
        assert len(lines) == 5

    def test_split_counts_sum_to_total(self, tmp_path):
        examples = [
            _make_example("Java", "http_rest", "DEFINITE_INWARD", i) for i in range(10)
        ]
        result = export_training_data(examples, tmp_path)

        assert (
            result.train_count + result.val_count + result.test_count
            == result.total_examples
        )

    def test_jsonl_records_are_valid_json(self, tmp_path):
        examples = [
            _make_example("Java", "http_rest", "DEFINITE_INWARD", i) for i in range(3)
        ]
        export_training_data(examples, tmp_path)

        for line in (tmp_path / "examples.jsonl").read_text().strip().split("\n"):
            record = json.loads(line)
            assert "id" in record
            assert "language" in record

    def test_deterministic_with_same_seed(self, tmp_path):
        examples = [
            _make_example("Java", "http_rest", label, i)
            for label in ["DEFINITE_INWARD", "DEFINITE_OUTWARD", "NOT_DEFINITE"]
            for i in range(5)
        ]

        dir1 = tmp_path / "run1"
        dir2 = tmp_path / "run2"
        export_training_data(examples, dir1, seed=42)
        export_training_data(examples, dir2, seed=42)

        assert (dir1 / "train.jsonl").read_text() == (dir2 / "train.jsonl").read_text()
        assert (dir1 / "val.jsonl").read_text() == (dir2 / "val.jsonl").read_text()
        assert (dir1 / "test.jsonl").read_text() == (dir2 / "test.jsonl").read_text()

    def test_different_seed_different_split(self, tmp_path):
        examples = [
            _make_example("Java", "http_rest", "DEFINITE_INWARD", i) for i in range(20)
        ]

        dir1 = tmp_path / "seed1"
        dir2 = tmp_path / "seed2"
        export_training_data(examples, dir1, seed=42)
        export_training_data(examples, dir2, seed=99)

        # Train files should differ (different shuffle)
        train1 = (dir1 / "train.jsonl").read_text()
        train2 = (dir2 / "train.jsonl").read_text()
        assert train1 != train2

    def test_creates_output_dir_if_missing(self, tmp_path):
        nested = tmp_path / "a" / "b" / "c"
        examples = [_make_example("Java", "http_rest", "DEFINITE_INWARD", 0)]
        export_training_data(examples, nested)
        assert nested.exists()

    def test_stratified_split_preserves_labels(self, tmp_path):
        examples = [
            _make_example("Java", "http_rest", label, i)
            for label in ["DEFINITE_INWARD", "DEFINITE_OUTWARD", "NOT_DEFINITE"]
            for i in range(6)
        ]
        export_training_data(examples, tmp_path)

        train_lines = (tmp_path / "train.jsonl").read_text().strip().split("\n")
        train_labels = {json.loads(line)["label"] for line in train_lines}
        # All 3 labels should be represented in training set
        assert len(train_labels) == 3
