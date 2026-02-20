"""Tests for signal grouping by AST context."""

from repo_surveyor.integration_concretiser.grouper import group_signals_by_ast_context
from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns import Confidence, IntegrationType, Language

JAVA_SOURCE = b"""\
package com.example;

import org.springframework.web.bind.annotation.*;

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

PYTHON_SOURCE = b"""\
import requests

def fetch_data(url):
    response = requests.get(url)
    return response.json()

def send_email(to, body):
    smtp.send(to, body)
"""


def _make_signal(
    file_path: str,
    line_number: int,
    line_content: str,
    language: Language,
    integration_type: IntegrationType = IntegrationType.HTTP_REST,
    pattern: str = "test_pattern",
) -> IntegrationSignal:
    return IntegrationSignal(
        match=FileMatch(
            file_path=file_path,
            line_number=line_number,
            line_content=line_content,
            language=language,
        ),
        integration_type=integration_type,
        confidence=Confidence.HIGH,
        matched_pattern=pattern,
        entity_type=EntityType.FILE_CONTENT,
        source="test",
    )


def _fake_file_reader(path: str) -> bytes:
    return {
        "UserController.java": JAVA_SOURCE,
        "client.py": PYTHON_SOURCE,
    }[path]


class TestGroupSignalsByASTContext:
    """Tests for signal grouping."""

    def test_signals_in_same_method_are_grouped(self):
        signals = [
            _make_signal(
                "UserController.java", 8, '@GetMapping("/users")', Language.JAVA
            ),
            _make_signal(
                "UserController.java",
                10,
                "return userService.findAll();",
                Language.JAVA,
            ),
        ]
        groups = group_signals_by_ast_context(signals, _fake_file_reader)
        assert len(groups) == 1
        assert len(groups[0].signals) == 2

    def test_signals_in_different_methods_are_separate(self):
        signals = [
            _make_signal(
                "UserController.java", 8, '@GetMapping("/users")', Language.JAVA
            ),
            _make_signal(
                "UserController.java", 13, '@PostMapping("/users")', Language.JAVA
            ),
        ]
        groups = group_signals_by_ast_context(signals, _fake_file_reader)
        assert len(groups) == 2

    def test_multi_file_signals_separated(self):
        signals = [
            _make_signal(
                "UserController.java", 8, '@GetMapping("/users")', Language.JAVA
            ),
            _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON),
        ]
        groups = group_signals_by_ast_context(signals, _fake_file_reader)
        assert len(groups) == 2
        file_paths = {g.file_path for g in groups}
        assert file_paths == {"UserController.java", "client.py"}

    def test_empty_signals_returns_empty(self):
        groups = group_signals_by_ast_context([], _fake_file_reader)
        assert groups == []

    def test_group_preserves_file_path(self):
        signals = [
            _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON),
        ]
        groups = group_signals_by_ast_context(signals, _fake_file_reader)
        assert len(groups) == 1
        assert groups[0].file_path == "client.py"

    def test_group_has_ast_context(self):
        signals = [
            _make_signal("client.py", 4, "requests.get(url)", Language.PYTHON),
        ]
        groups = group_signals_by_ast_context(signals, _fake_file_reader)
        assert groups[0].ast_context.node_type == "function_definition"
        assert "fetch_data" in groups[0].ast_context.node_text

    def test_signals_without_language_are_skipped(self):
        signal = IntegrationSignal(
            match=FileMatch(
                file_path="unknown.xyz",
                line_number=1,
                line_content="something",
                language=None,
            ),
            integration_type=IntegrationType.HTTP_REST,
            confidence=Confidence.HIGH,
            matched_pattern="test",
            entity_type=EntityType.FILE_CONTENT,
            source="test",
        )
        groups = group_signals_by_ast_context([signal], _fake_file_reader)
        assert groups == []
