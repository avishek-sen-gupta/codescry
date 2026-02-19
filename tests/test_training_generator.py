"""Tests for training data generator (parsing logic only, no API calls)."""

import json
from pathlib import Path

from repo_surveyor.training.generator import (
    _parse_generated_examples,
    _build_user_prompt,
    _make_example_id,
    _format_patterns,
    _load_checkpoint,
    _append_checkpoint,
    _triple_key,
    _build_batch_requests,
    _save_batch_checkpoint,
    _load_batch_checkpoint,
    _parse_batch_results,
    GenerationResult,
    generate_all,
    generate_all_batch,
)
from repo_surveyor.training.coverage import CoverageEntry
from repo_surveyor.training.types import TrainingExample, TrainingLabel
from repo_surveyor.integration_patterns.types import IntegrationType, Language
from repo_surveyor.ml_classifier.types import CompletionResult
from repo_surveyor.ml_classifier.claude_model import BatchResult
from repo_surveyor.ml_classifier.model_protocol import LLMModel


class TestMakeExampleId:
    def test_simple_language(self):
        result = _make_example_id("Java", "http_rest", "DEFINITE_INWARD", "Spring", 1)
        assert result == "java__http_rest__definite_inward__spring__001"

    def test_csharp_escaping(self):
        result = _make_example_id("C#", "database", "DEFINITE_OUTWARD", "EF Core", 2)
        assert result == "csharp__database__definite_outward__ef_core__002"

    def test_cpp_escaping(self):
        result = _make_example_id("C++", "socket", "NOT_DEFINITE", "stdlib", 3)
        assert result == "cpp__socket__not_definite__stdlib__003"

    def test_pli_escaping(self):
        result = _make_example_id("PL/I", "file_io", "DEFINITE_OUTWARD", "stdlib", 1)
        assert result == "pli__file_io__definite_outward__stdlib__001"


class TestFormatPatterns:
    def test_formats_numbered_list(self):
        result = _format_patterns(("@GetMapping", r"@PostMapping", r"RestTemplate"))
        assert "1. @GetMapping" in result
        assert "2. @PostMapping" in result
        assert "3. RestTemplate" in result


class TestBuildUserPrompt:
    def test_contains_language_and_type(self):
        prompt = _build_user_prompt(
            "Java",
            "http_rest",
            TrainingLabel.DEFINITE_INWARD,
            ("@GetMapping",),
            5,
        )
        assert "Java" in prompt
        assert "http_rest" in prompt
        assert "DEFINITE_INWARD" in prompt
        assert "@GetMapping" in prompt
        assert "5" in prompt

    def test_not_definite_guidance(self):
        prompt = _build_user_prompt(
            "Python",
            "database",
            TrainingLabel.NOT_DEFINITE,
            ("cursor.execute",),
            3,
        )
        assert "Type declarations" in prompt
        assert "Configuration" in prompt


class TestParseGeneratedExamples:
    def test_parses_valid_json_array(self):
        raw = json.dumps(
            [
                {
                    "code_snippet": '@GetMapping("/users")\npublic List<User> list() {}',
                    "signal_line_index": 0,
                    "signal_line_content": '@GetMapping("/users")',
                    "matched_pattern": "@GetMapping",
                    "ast_node_type": "method_declaration",
                    "framework": "Spring",
                }
            ]
        )
        examples = _parse_generated_examples(
            raw, "Java", "http_rest", "DEFINITE_INWARD"
        )
        assert len(examples) == 1
        assert examples[0].language == "Java"
        assert examples[0].integration_type == "http_rest"
        assert examples[0].label == "DEFINITE_INWARD"
        assert examples[0].framework == "Spring"

    def test_strips_markdown_fencing(self):
        inner = json.dumps(
            [
                {
                    "code_snippet": "code()",
                    "signal_line_index": 0,
                    "signal_line_content": "code()",
                    "matched_pattern": "code",
                    "ast_node_type": "call_expression",
                    "framework": "stdlib",
                }
            ]
        )
        raw = f"```json\n{inner}\n```"
        examples = _parse_generated_examples(
            raw, "Python", "database", "DEFINITE_OUTWARD"
        )
        assert len(examples) == 1

    def test_handles_non_array_response(self):
        examples = _parse_generated_examples(
            '{"not": "an array"}', "Java", "http_rest", "DEFINITE_INWARD"
        )
        assert examples == []

    def test_generates_sequential_ids(self):
        raw = json.dumps(
            [
                {
                    "code_snippet": "a()",
                    "signal_line_index": 0,
                    "signal_line_content": "a()",
                    "matched_pattern": "a",
                    "ast_node_type": "call",
                    "framework": "Spring",
                },
                {
                    "code_snippet": "b()",
                    "signal_line_index": 0,
                    "signal_line_content": "b()",
                    "matched_pattern": "b",
                    "ast_node_type": "call",
                    "framework": "Spring",
                },
            ]
        )
        examples = _parse_generated_examples(
            raw, "Java", "http_rest", "DEFINITE_INWARD"
        )
        assert examples[0].id.endswith("__001")
        assert examples[1].id.endswith("__002")

    def test_missing_framework_defaults_to_unknown(self):
        raw = json.dumps(
            [
                {
                    "code_snippet": "x()",
                    "signal_line_index": 0,
                    "signal_line_content": "x()",
                    "matched_pattern": "x",
                    "ast_node_type": "call",
                }
            ]
        )
        examples = _parse_generated_examples(raw, "Go", "grpc", "DEFINITE_OUTWARD")
        assert examples[0].framework == "unknown"


def _make_test_example(
    language: str, integration_type: str, label: str
) -> TrainingExample:
    return TrainingExample(
        id=f"{language}__{integration_type}__{label}__spring__001",
        language=language,
        integration_type=integration_type,
        label=label,
        code_snippet="code()",
        signal_line_index=0,
        signal_line_content="code()",
        matched_pattern="code",
        ast_node_type="call",
        framework="Spring",
    )


def _make_test_result(
    language: str, integration_type: str, label: str
) -> GenerationResult:
    return GenerationResult(
        language=language,
        integration_type=integration_type,
        label=label,
        examples=(_make_test_example(language, integration_type, label),),
        prompt_tokens=100,
        completion_tokens=200,
    )


class _FakeLLMModel:
    """Fake LLM model that returns canned JSON responses."""

    def __init__(self):
        self.call_count = 0

    @property
    def model_id(self) -> str:
        return "fake-model"

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        self.call_count += 1
        response = json.dumps(
            [
                {
                    "code_snippet": f"code_{self.call_count}()",
                    "signal_line_index": 0,
                    "signal_line_content": f"code_{self.call_count}()",
                    "matched_pattern": "code",
                    "ast_node_type": "call",
                    "framework": "Spring",
                }
            ]
        )
        return CompletionResult(text=response, prompt_tokens=50, completion_tokens=100)


class TestTripleKey:
    def test_builds_key(self):
        assert (
            _triple_key("Java", "http_rest", "DEFINITE_INWARD")
            == "Java__http_rest__DEFINITE_INWARD"
        )


class TestCheckpoint:
    def test_load_empty_checkpoint(self, tmp_path):
        path = tmp_path / "checkpoint.jsonl"
        assert _load_checkpoint(path) == []

    def test_append_and_load_checkpoint(self, tmp_path):
        path = tmp_path / "checkpoint.jsonl"
        result = _make_test_result("Java", "http_rest", "DEFINITE_INWARD")

        _append_checkpoint(path, result)
        loaded = _load_checkpoint(path)

        assert len(loaded) == 1
        assert loaded[0].language == "Java"
        assert loaded[0].integration_type == "http_rest"
        assert loaded[0].label == "DEFINITE_INWARD"
        assert loaded[0].prompt_tokens == 100
        assert loaded[0].completion_tokens == 200
        assert len(loaded[0].examples) == 1
        assert loaded[0].examples[0].language == "Java"

    def test_append_multiple_and_load(self, tmp_path):
        path = tmp_path / "checkpoint.jsonl"
        _append_checkpoint(
            path, _make_test_result("Java", "http_rest", "DEFINITE_INWARD")
        )
        _append_checkpoint(
            path, _make_test_result("Python", "database", "DEFINITE_OUTWARD")
        )

        loaded = _load_checkpoint(path)
        assert len(loaded) == 2
        assert loaded[0].language == "Java"
        assert loaded[1].language == "Python"


class TestGenerateAllResume:
    def _make_entry(
        self, language: Language, integration_type: IntegrationType
    ) -> CoverageEntry:
        return CoverageEntry(
            language=language,
            integration_type=integration_type,
            pattern_count=1,
            sample_patterns=("(?i)\\btest\\b",),
        )

    def test_fresh_run_creates_checkpoint(self, tmp_path):
        model = _FakeLLMModel()
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)
        checkpoint_path = tmp_path / "checkpoint.jsonl"

        results = generate_all(
            model=model,
            entries=[entry],
            examples_per_triple=1,
            checkpoint_path=checkpoint_path,
            resume=False,
        )

        assert len(results) == len(TrainingLabel)
        assert checkpoint_path.exists()
        loaded = _load_checkpoint(checkpoint_path)
        assert len(loaded) == len(TrainingLabel)

    def test_resume_skips_completed_triples(self, tmp_path):
        model = _FakeLLMModel()
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)
        checkpoint_path = tmp_path / "checkpoint.jsonl"

        # First run: generate all triples
        generate_all(
            model=model,
            entries=[entry],
            examples_per_triple=1,
            checkpoint_path=checkpoint_path,
            resume=False,
        )
        calls_after_first_run = model.call_count

        # Second run with resume: should skip all
        results = generate_all(
            model=model,
            entries=[entry],
            examples_per_triple=1,
            checkpoint_path=checkpoint_path,
            resume=True,
        )

        assert model.call_count == calls_after_first_run  # No new API calls
        assert len(results) == len(TrainingLabel)

    def test_resume_generates_remaining_triples(self, tmp_path):
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)
        checkpoint_path = tmp_path / "checkpoint.jsonl"

        # Manually checkpoint only the first label
        first_label = list(TrainingLabel)[0]
        _append_checkpoint(
            checkpoint_path,
            _make_test_result("Java", "http_rest", first_label.value),
        )

        model = _FakeLLMModel()
        results = generate_all(
            model=model,
            entries=[entry],
            examples_per_triple=1,
            checkpoint_path=checkpoint_path,
            resume=True,
        )

        remaining_labels = len(TrainingLabel) - 1
        assert model.call_count == remaining_labels
        assert len(results) == len(TrainingLabel)

    def test_fresh_run_truncates_existing_checkpoint(self, tmp_path):
        checkpoint_path = tmp_path / "checkpoint.jsonl"
        _append_checkpoint(
            checkpoint_path,
            _make_test_result("Java", "http_rest", "DEFINITE_INWARD"),
        )

        model = _FakeLLMModel()
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)

        results = generate_all(
            model=model,
            entries=[entry],
            examples_per_triple=1,
            checkpoint_path=checkpoint_path,
            resume=False,
        )

        assert model.call_count == len(TrainingLabel)  # All generated fresh
        loaded = _load_checkpoint(checkpoint_path)
        assert len(loaded) == len(TrainingLabel)


class TestBuildBatchRequests:
    def test_builds_requests_for_all_triples(self):
        entry = CoverageEntry(
            language=Language.JAVA,
            integration_type=IntegrationType.HTTP_REST,
            pattern_count=1,
            sample_patterns=("@GetMapping",),
        )
        requests = _build_batch_requests([entry], examples_per_triple=3)

        assert len(requests) == len(TrainingLabel)
        custom_ids = [r[0] for r in requests]
        assert "Java__http_rest__DEFINITE_INWARD" in custom_ids
        assert "Java__http_rest__DEFINITE_OUTWARD" in custom_ids
        assert "Java__http_rest__NOT_DEFINITE" in custom_ids

    def test_all_requests_share_system_prompt(self):
        entry = CoverageEntry(
            language=Language.PYTHON,
            integration_type=IntegrationType.DATABASE,
            pattern_count=1,
            sample_patterns=("cursor.execute",),
        )
        requests = _build_batch_requests([entry], examples_per_triple=2)
        system_prompts = {r[1] for r in requests}
        assert len(system_prompts) == 1

    def test_user_prompt_contains_count(self):
        entry = CoverageEntry(
            language=Language.JAVA,
            integration_type=IntegrationType.HTTP_REST,
            pattern_count=1,
            sample_patterns=("@GetMapping",),
        )
        requests = _build_batch_requests([entry], examples_per_triple=7)
        for _, _, user_prompt in requests:
            assert "7" in user_prompt

    def test_multiple_entries(self):
        entries = [
            CoverageEntry(
                language=Language.JAVA,
                integration_type=IntegrationType.HTTP_REST,
                pattern_count=1,
                sample_patterns=("@GetMapping",),
            ),
            CoverageEntry(
                language=Language.PYTHON,
                integration_type=IntegrationType.DATABASE,
                pattern_count=1,
                sample_patterns=("cursor.execute",),
            ),
        ]
        requests = _build_batch_requests(entries, examples_per_triple=5)
        assert len(requests) == 2 * len(TrainingLabel)


class TestBatchCheckpoint:
    def test_save_and_load(self, tmp_path):
        path = tmp_path / "batch_checkpoint.json"
        _save_batch_checkpoint(path, "batch_abc123")
        loaded = _load_batch_checkpoint(path)
        assert loaded == "batch_abc123"

    def test_load_missing_file(self, tmp_path):
        path = tmp_path / "nonexistent.json"
        assert _load_batch_checkpoint(path) == ""

    def test_save_creates_parent_dirs(self, tmp_path):
        path = tmp_path / "sub" / "dir" / "checkpoint.json"
        _save_batch_checkpoint(path, "batch_xyz")
        assert _load_batch_checkpoint(path) == "batch_xyz"


class TestParseBatchResults:
    def _make_entry(
        self, language: Language, integration_type: IntegrationType
    ) -> CoverageEntry:
        return CoverageEntry(
            language=language,
            integration_type=integration_type,
            pattern_count=1,
            sample_patterns=("(?i)\\btest\\b",),
        )

    def _make_batch_result(self, custom_id: str, succeeded: bool = True) -> BatchResult:
        response_json = json.dumps(
            [
                {
                    "code_snippet": f"code_{custom_id}()",
                    "signal_line_index": 0,
                    "signal_line_content": f"code_{custom_id}()",
                    "matched_pattern": "code",
                    "ast_node_type": "call",
                    "framework": "Spring",
                }
            ]
        )
        return BatchResult(
            custom_id=custom_id,
            completion=CompletionResult(
                text=response_json if succeeded else "",
                prompt_tokens=50,
                completion_tokens=100,
            ),
            succeeded=succeeded,
        )

    def test_parses_all_succeeded_results(self):
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)
        batch_results = {
            _triple_key("Java", "http_rest", label.value): self._make_batch_result(
                _triple_key("Java", "http_rest", label.value)
            )
            for label in TrainingLabel
        }

        results = _parse_batch_results(batch_results, [entry])
        assert len(results) == len(TrainingLabel)
        for r in results:
            assert r.language == "Java"
            assert r.integration_type == "http_rest"
            assert len(r.examples) == 1

    def test_skips_failed_results(self):
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)
        labels = list(TrainingLabel)
        batch_results = {
            _triple_key("Java", "http_rest", labels[0].value): self._make_batch_result(
                _triple_key("Java", "http_rest", labels[0].value), succeeded=False
            ),
            _triple_key("Java", "http_rest", labels[1].value): self._make_batch_result(
                _triple_key("Java", "http_rest", labels[1].value)
            ),
            _triple_key("Java", "http_rest", labels[2].value): self._make_batch_result(
                _triple_key("Java", "http_rest", labels[2].value)
            ),
        }

        results = _parse_batch_results(batch_results, [entry])
        assert len(results) == len(TrainingLabel) - 1

    def test_skips_missing_results(self):
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)
        first_label = list(TrainingLabel)[0]
        batch_results = {
            _triple_key(
                "Java", "http_rest", first_label.value
            ): self._make_batch_result(
                _triple_key("Java", "http_rest", first_label.value)
            ),
        }

        results = _parse_batch_results(batch_results, [entry])
        assert len(results) == 1


class _FakeBatchModel:
    """Fake model that records batch calls for testing generate_all_batch."""

    def __init__(self):
        self.created_requests: list[tuple[str, str, str]] = []
        self.batch_id = "fake_batch_001"
        self.poll_count = 0

    @property
    def model_id(self) -> str:
        return "fake-batch-model"

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        raise NotImplementedError("Batch model should not call classify")

    def create_batch(
        self, requests: list[tuple[str, str, str]], temperature: float = 0.0
    ) -> str:
        self.created_requests = list(requests)
        return self.batch_id

    def poll_batch(self, batch_id: str) -> None:
        self.poll_count += 1

    def retrieve_batch_results(self, batch_id: str) -> dict[str, BatchResult]:
        results: dict[str, BatchResult] = {}
        for custom_id, _, _ in self.created_requests:
            response_json = json.dumps(
                [
                    {
                        "code_snippet": f"code_{custom_id}()",
                        "signal_line_index": 0,
                        "signal_line_content": f"code_{custom_id}()",
                        "matched_pattern": "code",
                        "ast_node_type": "call",
                        "framework": "Spring",
                    }
                ]
            )
            results[custom_id] = BatchResult(
                custom_id=custom_id,
                completion=CompletionResult(
                    text=response_json, prompt_tokens=50, completion_tokens=100
                ),
                succeeded=True,
            )
        return results


class TestGenerateAllBatch:
    def _make_entry(
        self, language: Language, integration_type: IntegrationType
    ) -> CoverageEntry:
        return CoverageEntry(
            language=language,
            integration_type=integration_type,
            pattern_count=1,
            sample_patterns=("(?i)\\btest\\b",),
        )

    def test_generates_all_triples(self):
        model = _FakeBatchModel()
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)

        results = generate_all_batch(
            model=model,
            entries=[entry],
            examples_per_triple=1,
        )

        assert len(results) == len(TrainingLabel)
        assert len(model.created_requests) == len(TrainingLabel)
        assert model.poll_count == 1

    def test_saves_batch_checkpoint(self, tmp_path):
        model = _FakeBatchModel()
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)
        checkpoint = tmp_path / "batch_checkpoint.json"

        generate_all_batch(
            model=model,
            entries=[entry],
            examples_per_triple=1,
            batch_checkpoint_path=checkpoint,
        )

        assert checkpoint.exists()
        loaded_id = _load_batch_checkpoint(checkpoint)
        assert loaded_id == "fake_batch_001"

    def test_resume_skips_submission(self, tmp_path):
        checkpoint = tmp_path / "batch_checkpoint.json"
        _save_batch_checkpoint(checkpoint, "existing_batch_id")

        model = _FakeBatchModel()
        model.created_requests = [
            (_triple_key("Java", "http_rest", label.value), "", "")
            for label in TrainingLabel
        ]
        entry = self._make_entry(Language.JAVA, IntegrationType.HTTP_REST)

        results = generate_all_batch(
            model=model,
            entries=[entry],
            examples_per_triple=1,
            batch_checkpoint_path=checkpoint,
            resume=True,
        )

        assert len(results) == len(TrainingLabel)
        assert model.poll_count == 1
