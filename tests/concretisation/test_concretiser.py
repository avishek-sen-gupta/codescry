"""Tests for ML-based integration signal concretisation."""

from repo_surveyor.integration_concretiser import (
    ConcretisationResult,
    ConcretisedSignal,
    SignalGroup,
    concretise_integration_signals,
)
from repo_surveyor.integration_concretiser.concretiser import concretise_groups
from repo_surveyor.integration_concretiser.types import ASTContext
from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language
from repo_surveyor.training.types import TrainingLabel


class _StubClassifier:
    """Stub SignalClassifier that returns a fixed label per line content."""

    def __init__(self, label_map: dict[str, TrainingLabel]) -> None:
        self._label_map = label_map

    def predict(self, signal_line: str) -> TrainingLabel:
        return self._label_map.get(signal_line, TrainingLabel.NOT_DEFINITE)


_PYTHON_SOURCE = b"""\
import requests

def fetch_data(url):
    response = requests.get(url)
    return response.json()

def send_email(to, body):
    smtp.send(to, body)
"""

_JAVA_SOURCE = b"""\
package com.example;

@RestController
public class UserController {

    @GetMapping("/users")
    public List<User> getUsers() {
        return userService.findAll();
    }

    @PostMapping("/users")
    public User createUser(@RequestBody User user) {
        return userService.save(user);
    }
}
"""

_FAKE_FILES: dict[str, bytes] = {
    "client.py": _PYTHON_SOURCE,
    "UserController.java": _JAVA_SOURCE,
}


def _fake_reader(path: str) -> bytes:
    return _FAKE_FILES[path]


def _make_signal(
    file_path: str,
    line_number: int,
    line_content: str,
    language: Language,
    entity_type: EntityType = EntityType.FILE_CONTENT,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content=line_content,
            language=language,
        ),
        integration_type=IntegrationType.HTTP_REST,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=entity_type,
        source="test",
    )


def _make_ast_context(node_text: str = "def fetch_data(url): ...") -> ASTContext:
    return ASTContext(
        node_type="function_definition",
        node_text=node_text,
        start_line=3,
        end_line=5,
    )


class TestConcretiseGroups:
    """Unit tests for concretise_groups using stub classifier."""

    def test_single_signal_label_propagated(self):
        signal = _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON)
        ctx = _make_ast_context()
        group = SignalGroup(ast_context=ctx, signals=(signal,), file_path="client.py")
        classifier = _StubClassifier(
            {"requests.get(url)": TrainingLabel.DEFINITE_INWARD}
        )

        result = concretise_groups([group], classifier)

        assert len(result.concretised) == 1
        assert result.concretised[0].label == TrainingLabel.DEFINITE_INWARD
        assert result.concretised[0].original_signal is signal
        assert result.concretised[0].ast_context is ctx

    def test_is_definite_true_for_inward(self):
        signal = _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON)
        group = SignalGroup(
            ast_context=_make_ast_context(),
            signals=(signal,),
            file_path="client.py",
        )
        classifier = _StubClassifier(
            {"requests.get(url)": TrainingLabel.DEFINITE_INWARD}
        )

        result = concretise_groups([group], classifier)

        assert result.concretised[0].is_definite is True

    def test_is_definite_true_for_outward(self):
        signal = _make_signal("client.py", 8, "smtp.send(to, body)", Language.PYTHON)
        group = SignalGroup(
            ast_context=_make_ast_context(),
            signals=(signal,),
            file_path="client.py",
        )
        classifier = _StubClassifier(
            {"smtp.send(to, body)": TrainingLabel.DEFINITE_OUTWARD}
        )

        result = concretise_groups([group], classifier)

        assert result.concretised[0].is_definite is True

    def test_is_definite_false_for_not_definite(self):
        signal = _make_signal("client.py", 4, "unknown_line", Language.PYTHON)
        group = SignalGroup(
            ast_context=_make_ast_context(),
            signals=(signal,),
            file_path="client.py",
        )
        classifier = _StubClassifier({})  # defaults to NOT_DEFINITE

        result = concretise_groups([group], classifier)

        assert result.concretised[0].is_definite is False

    def test_counts_are_correct(self):
        inward_line = "requests.get(url)"
        outward_line = "smtp.send(to, body)"
        unknown_line = "print('hello')"

        signals = (
            _make_signal("client.py", 4, inward_line, Language.PYTHON),
            _make_signal("client.py", 8, outward_line, Language.PYTHON),
            _make_signal("client.py", 9, unknown_line, Language.PYTHON),
        )
        ctx = _make_ast_context()
        group = SignalGroup(ast_context=ctx, signals=signals, file_path="client.py")
        classifier = _StubClassifier(
            {
                inward_line: TrainingLabel.DEFINITE_INWARD,
                outward_line: TrainingLabel.DEFINITE_OUTWARD,
            }
        )

        result = concretise_groups([group], classifier)

        assert result.signals_submitted == 3
        assert result.signals_definite == 2
        assert result.signals_discarded == 1

    def test_signals_from_multiple_groups_all_included(self):
        signal_a = _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON)
        signal_b = _make_signal("client.py", 8, "smtp.send(to, body)", Language.PYTHON)
        ctx_a = _make_ast_context("def fetch_data(url): ...")
        ctx_b = ASTContext(
            node_type="function_definition",
            node_text="def send_email(to, body): ...",
            start_line=7,
            end_line=8,
        )
        groups = [
            SignalGroup(ast_context=ctx_a, signals=(signal_a,), file_path="client.py"),
            SignalGroup(ast_context=ctx_b, signals=(signal_b,), file_path="client.py"),
        ]
        classifier = _StubClassifier(
            {
                "requests.get(url)": TrainingLabel.DEFINITE_INWARD,
                "smtp.send(to, body)": TrainingLabel.DEFINITE_OUTWARD,
            }
        )

        result = concretise_groups(groups, classifier)

        assert result.signals_submitted == 2
        assert result.signals_definite == 2
        assert result.signals_discarded == 0
        assert len(result.concretised) == 2

    def test_empty_groups_returns_empty_result(self):
        classifier = _StubClassifier({})
        result = concretise_groups([], classifier)
        assert result.signals_submitted == 0
        assert result.signals_definite == 0
        assert result.signals_discarded == 0
        assert result.concretised == ()


class TestConcretiseIntegrationSignals:
    """Integration tests for the top-level concretise_integration_signals."""

    def test_filters_out_directory_signals(self):
        file_signal = _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON)
        dir_signal = _make_signal(
            "client.py",
            4,
            "requests.get(url)",
            Language.PYTHON,
            entity_type=EntityType.DIRECTORY,
        )
        detector_result = IntegrationDetectorResult(
            integration_points=[file_signal, dir_signal],
            files_scanned=1,
        )
        classifier = _StubClassifier(
            {"requests.get(url)": TrainingLabel.DEFINITE_INWARD}
        )

        result = concretise_integration_signals(
            detector_result, classifier, _fake_reader
        )

        assert result.signals_submitted == 1

    def test_end_to_end_with_real_file_reader(self):
        signal = _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON)
        detector_result = IntegrationDetectorResult(
            integration_points=[signal],
            files_scanned=1,
        )
        classifier = _StubClassifier(
            {"requests.get(url)": TrainingLabel.DEFINITE_INWARD}
        )

        result = concretise_integration_signals(
            detector_result, classifier, _fake_reader
        )

        assert result.signals_submitted == 1
        assert result.signals_definite == 1
        assert result.concretised[0].label == TrainingLabel.DEFINITE_INWARD
