"""End-to-end tests for integration signal concretiser with mock model."""

from dataclasses import dataclass

from repo_surveyor.integration_concretiser.concretiser import concretise_groups
from repo_surveyor.integration_concretiser.grouper import SignalGroup
from repo_surveyor.integration_concretiser.types import (
    ASTContext,
    IntegrationDirection,
)
from repo_surveyor.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language
from repo_surveyor.ml_classifier.types import CompletionResult


@dataclass
class MockModel:
    """Mock LLM model for testing."""

    responses: list[str]
    _call_count: int = 0

    @property
    def model_id(self) -> str:
        return "mock-model"

    def classify(self, system_prompt: str, user_prompt: str) -> CompletionResult:
        response = self.responses[self._call_count % len(self.responses)]
        self._call_count += 1
        return CompletionResult(text=response, prompt_tokens=100, completion_tokens=50)


def _make_signal(
    file_path: str,
    line_number: int,
    integration_type: IntegrationType,
    language: Language = Language.JAVA,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content=f"line {line_number}",
            language=language,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern="test_pattern",
        entity_type=EntityType.FILE_CONTENT,
        source="test",
    )


SAMPLE_CONTEXT = ASTContext(
    node_type="method_declaration",
    node_text="public void handle() { }",
    start_line=5,
    end_line=7,
)


class TestConcretiseGroups:
    """End-to-end tests with mock model."""

    def test_single_group_all_definite(self):
        group = SignalGroup(
            ast_context=SAMPLE_CONTEXT,
            signals=(
                _make_signal("A.java", 5, IntegrationType.HTTP_REST),
                _make_signal("A.java", 6, IntegrationType.DATABASE),
            ),
            file_path="A.java",
        )
        model = MockModel(
            responses=["0|DEFINITE|INWARD|HTTP handler\n1|DEFINITE|OUTWARD|DB query"]
        )
        result = concretise_groups([group], model)

        assert result.signals_submitted == 2
        assert result.signals_definite == 2
        assert result.signals_discarded == 0
        assert len(result.concretised) == 2
        assert result.concretised[0].direction == IntegrationDirection.INWARD
        assert result.concretised[1].direction == IntegrationDirection.OUTWARD

    def test_single_group_mixed_results(self):
        group = SignalGroup(
            ast_context=SAMPLE_CONTEXT,
            signals=(
                _make_signal("A.java", 5, IntegrationType.HTTP_REST),
                _make_signal("A.java", 6, IntegrationType.HTTP_REST),
            ),
            file_path="A.java",
        )
        model = MockModel(
            responses=["0|DEFINITE|INWARD|Handler\n1|NOT_DEFINITE||Import only"]
        )
        result = concretise_groups([group], model)

        assert result.signals_submitted == 2
        assert result.signals_definite == 1
        assert result.signals_discarded == 1

    def test_multiple_groups(self):
        group_a = SignalGroup(
            ast_context=SAMPLE_CONTEXT,
            signals=(_make_signal("A.java", 5, IntegrationType.HTTP_REST),),
            file_path="A.java",
        )
        group_b = SignalGroup(
            ast_context=ASTContext(
                node_type="function_definition",
                node_text="def send(): pass",
                start_line=1,
                end_line=1,
            ),
            signals=(
                _make_signal("B.py", 1, IntegrationType.MESSAGING, Language.PYTHON),
            ),
            file_path="B.py",
        )
        model = MockModel(
            responses=[
                "0|DEFINITE|INWARD|HTTP handler",
                "0|DEFINITE|OUTWARD|Message publish",
            ]
        )
        result = concretise_groups([group_a, group_b], model)

        assert result.signals_submitted == 2
        assert result.signals_definite == 2
        assert len(result.concretised) == 2

    def test_empty_groups(self):
        model = MockModel(responses=["not used"])
        result = concretise_groups([], model)

        assert result.signals_submitted == 0
        assert result.signals_definite == 0
        assert result.signals_discarded == 0
        assert result.concretised == ()

    def test_malformed_response_defaults_to_not_definite(self):
        group = SignalGroup(
            ast_context=SAMPLE_CONTEXT,
            signals=(_make_signal("A.java", 5, IntegrationType.HTTP_REST),),
            file_path="A.java",
        )
        model = MockModel(responses=["completely invalid response"])
        result = concretise_groups([group], model)

        assert result.signals_submitted == 1
        assert result.signals_definite == 0
        assert result.signals_discarded == 1
        assert result.concretised[0].is_definite is False

    def test_preserves_original_signals(self):
        signal = _make_signal("A.java", 5, IntegrationType.HTTP_REST)
        group = SignalGroup(
            ast_context=SAMPLE_CONTEXT,
            signals=(signal,),
            file_path="A.java",
        )
        model = MockModel(responses=["0|DEFINITE|INWARD|Handler"])
        result = concretise_groups([group], model)

        assert result.concretised[0].original_signal is signal
