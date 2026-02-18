"""Tests for concretiser prompt construction."""

from repo_surveyor.integration_concretiser.prompt import (
    build_system_prompt,
    build_user_prompt,
)
from repo_surveyor.integration_concretiser.grouper import SignalGroup
from repo_surveyor.integration_concretiser.types import ASTContext
from repo_surveyor.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language


def _make_signal(
    line_number: int,
    line_content: str,
    integration_type: IntegrationType,
    pattern: str,
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path="MyService.java",
            line_number=line_number,
            line_content=line_content,
            language=Language.JAVA,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern=pattern,
        entity_type=EntityType.FILE_CONTENT,
        source="test",
    )


SAMPLE_GROUP = SignalGroup(
    ast_context=ASTContext(
        node_type="method_declaration",
        node_text='@GetMapping("/users")\npublic List<User> getUsers() {\n    return userService.findAll();\n}',
        start_line=8,
        end_line=11,
    ),
    signals=(
        _make_signal(
            8, '@GetMapping("/users")', IntegrationType.HTTP_REST, "@GetMapping"
        ),
        _make_signal(
            10, "return userService.findAll();", IntegrationType.DATABASE, "findAll"
        ),
    ),
    file_path="MyService.java",
)


class TestBuildSystemPrompt:
    """Tests for system prompt construction."""

    def test_system_prompt_mentions_inward_outward(self):
        prompt = build_system_prompt()
        assert "INWARD" in prompt
        assert "OUTWARD" in prompt

    def test_system_prompt_mentions_not_definite(self):
        prompt = build_system_prompt()
        assert "NOT_DEFINITE" in prompt

    def test_system_prompt_describes_output_format(self):
        prompt = build_system_prompt()
        assert "SIGNAL_INDEX" in prompt
        assert "DIRECTION" in prompt
        assert "BRIEF_REASON" in prompt

    def test_system_prompt_mentions_imports_not_definite(self):
        prompt = build_system_prompt()
        assert "import" in prompt.lower()


class TestBuildUserPrompt:
    """Tests for user prompt construction."""

    def test_includes_node_type(self):
        prompt = build_user_prompt(SAMPLE_GROUP)
        assert "method_declaration" in prompt

    def test_includes_file_path(self):
        prompt = build_user_prompt(SAMPLE_GROUP)
        assert "MyService.java" in prompt

    def test_includes_code_block(self):
        prompt = build_user_prompt(SAMPLE_GROUP)
        assert "@GetMapping" in prompt
        assert "getUsers" in prompt

    def test_includes_signal_list_with_indices(self):
        prompt = build_user_prompt(SAMPLE_GROUP)
        assert "0." in prompt
        assert "1." in prompt

    def test_includes_integration_types(self):
        prompt = build_user_prompt(SAMPLE_GROUP)
        assert "http_rest" in prompt
        assert "database" in prompt

    def test_includes_matched_patterns(self):
        prompt = build_user_prompt(SAMPLE_GROUP)
        assert "@GetMapping" in prompt
        assert "findAll" in prompt

    def test_includes_line_numbers(self):
        prompt = build_user_prompt(SAMPLE_GROUP)
        assert "Line 8" in prompt
        assert "Line 10" in prompt

    def test_single_signal_group(self):
        group = SignalGroup(
            ast_context=ASTContext(
                node_type="function_definition",
                node_text="def foo():\n    pass",
                start_line=1,
                end_line=2,
            ),
            signals=(_make_signal(1, "def foo():", IntegrationType.HTTP_REST, "foo"),),
            file_path="test.py",
        )
        prompt = build_user_prompt(group)
        assert "0." in prompt
        assert "1." not in prompt
