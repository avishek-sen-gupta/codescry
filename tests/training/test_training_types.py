"""Tests for training data types."""

import json

from repo_surveyor.training.types import TrainingExample, TrainingLabel


class TestTrainingLabel:
    def test_has_4_members(self):
        assert len(TrainingLabel) == 4

    def test_values(self):
        assert TrainingLabel.DEFINITE_INWARD.value == "DEFINITE_INWARD"
        assert TrainingLabel.DEFINITE_OUTWARD.value == "DEFINITE_OUTWARD"
        assert TrainingLabel.NOT_DEFINITE.value == "NOT_DEFINITE"


class TestTrainingExample:
    def _make_example(self, **overrides):
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

    def test_is_frozen(self):
        ex = self._make_example()
        try:
            ex.id = "other"  # type: ignore[misc]
            assert False, "Should have raised"
        except AttributeError:
            pass

    def test_fields(self):
        ex = self._make_example()
        assert ex.id == "java__http_rest__definite_inward__spring__001"
        assert ex.language == "Java"
        assert ex.integration_type == "http_rest"
        assert ex.label == "DEFINITE_INWARD"
        assert ex.signal_line_index == 0
        assert ex.framework == "Spring"

    def test_to_dict(self):
        ex = self._make_example()
        d = ex.to_dict()
        assert isinstance(d, dict)
        assert d["id"] == ex.id
        assert d["language"] == "Java"
        assert d["signal_line_index"] == 0

    def test_to_dict_roundtrip(self):
        ex = self._make_example()
        d = ex.to_dict()
        json_str = json.dumps(d)
        parsed = json.loads(json_str)
        restored = TrainingExample(**parsed)
        assert restored == ex

    def test_to_dict_preserves_newlines_in_snippet(self):
        ex = self._make_example(code_snippet="line1\nline2\nline3")
        d = ex.to_dict()
        json_str = json.dumps(d)
        parsed = json.loads(json_str)
        assert parsed["code_snippet"] == "line1\nline2\nline3"

    def test_equality(self):
        ex1 = self._make_example()
        ex2 = self._make_example()
        assert ex1 == ex2

    def test_inequality_on_different_id(self):
        ex1 = self._make_example(id="a")
        ex2 = self._make_example(id="b")
        assert ex1 != ex2
