"""Tests for training data validator."""

from repo_surveyor.training.types import TrainingExample
from repo_surveyor.training.validator import (
    ValidationFailure,
    validate_example,
    validate_batch,
)


def _make_example(**overrides) -> TrainingExample:
    """Create a valid TrainingExample with optional overrides."""
    defaults = {
        "id": "java__http_rest__definite_inward__spring__001",
        "language": "Java",
        "integration_type": "http_rest",
        "label": "DEFINITE_INWARD",
        "code_snippet": '@GetMapping("/users")\npublic List<User> getUsers() {\n    return userService.findAll();\n}',
        "signal_line_index": 0,
        "signal_line_content": '@GetMapping("/users")',
        "matched_pattern": "@GetMapping",
        "ast_node_type": "method_declaration",
        "framework": "Spring",
    }
    defaults.update(overrides)
    return TrainingExample(**defaults)


class TestSchemaValidation:
    def test_valid_example_passes(self):
        result = validate_example(_make_example())
        assert result.is_valid
        assert result.failures == ()

    def test_empty_id_fails(self):
        result = validate_example(_make_example(id=""))
        assert not result.is_valid
        assert ValidationFailure.MISSING_FIELD in result.failures

    def test_empty_code_snippet_fails(self):
        result = validate_example(_make_example(code_snippet=""))
        assert not result.is_valid
        assert ValidationFailure.MISSING_FIELD in result.failures

    def test_invalid_label_fails(self):
        result = validate_example(_make_example(label="BOGUS"))
        assert not result.is_valid
        assert ValidationFailure.INVALID_LABEL in result.failures

    def test_invalid_language_fails(self):
        result = validate_example(_make_example(language="Brainfuck"))
        assert not result.is_valid
        assert ValidationFailure.INVALID_LANGUAGE in result.failures

    def test_invalid_integration_type_fails(self):
        result = validate_example(
            _make_example(integration_type="quantum_entanglement")
        )
        assert not result.is_valid
        assert ValidationFailure.INVALID_INTEGRATION_TYPE in result.failures

    def test_signal_index_out_of_bounds_fails(self):
        result = validate_example(_make_example(signal_line_index=99))
        assert not result.is_valid
        assert ValidationFailure.SIGNAL_INDEX_OUT_OF_BOUNDS in result.failures

    def test_negative_signal_index_fails(self):
        result = validate_example(_make_example(signal_line_index=-1))
        assert not result.is_valid
        assert ValidationFailure.SIGNAL_INDEX_OUT_OF_BOUNDS in result.failures

    def test_signal_content_mismatch_fails(self):
        result = validate_example(
            _make_example(signal_line_content="something completely different")
        )
        assert not result.is_valid
        assert ValidationFailure.SIGNAL_CONTENT_MISMATCH in result.failures

    def test_signal_content_stripped_comparison(self):
        result = validate_example(
            _make_example(
                code_snippet='  @GetMapping("/users")  \nreturn x;',
                signal_line_index=0,
                signal_line_content='@GetMapping("/users")',
            )
        )
        assert result.is_valid


class TestRegexValidation:
    def test_matched_pattern_actually_matches(self):
        result = validate_example(_make_example())
        assert result.is_valid

    def test_matched_pattern_no_match_fails(self):
        result = validate_example(_make_example(matched_pattern=r"@PostMapping"))
        assert not result.is_valid
        assert ValidationFailure.MATCHED_PATTERN_NO_MATCH in result.failures

    def test_invalid_regex_fails(self):
        result = validate_example(_make_example(matched_pattern="[invalid("))
        assert not result.is_valid
        assert ValidationFailure.MATCHED_PATTERN_NO_MATCH in result.failures

    def test_registered_pattern_matches(self):
        # @GetMapping is a Spring HTTP_REST pattern — should pass
        result = validate_example(_make_example())
        assert result.is_valid

    def test_no_registered_pattern_match_fails(self):
        # Use a signal line that matches the claimed pattern but no registered pattern
        result = validate_example(
            _make_example(
                signal_line_content="xyzUnknownCall()",
                code_snippet="xyzUnknownCall()\nreturn 1;",
                signal_line_index=0,
                matched_pattern="xyzUnknownCall",
            )
        )
        assert not result.is_valid
        assert ValidationFailure.NO_REGISTERED_PATTERN_MATCH in result.failures


class TestBatchValidation:
    def test_all_valid(self):
        examples = [_make_example(id=f"ex_{i}") for i in range(3)]
        result = validate_batch(examples)
        assert result.total == 3
        assert result.valid == 3
        assert result.invalid == 0
        assert len(result.valid_examples) == 3

    def test_mixed_valid_and_invalid(self):
        good = _make_example(id="good")
        bad = _make_example(id="bad", label="BOGUS")
        result = validate_batch([good, bad])
        assert result.total == 2
        assert result.valid == 1
        assert result.invalid == 1
        assert result.valid_examples == (good,)

    def test_empty_batch(self):
        result = validate_batch([])
        assert result.total == 0
        assert result.valid == 0
        assert result.invalid == 0
