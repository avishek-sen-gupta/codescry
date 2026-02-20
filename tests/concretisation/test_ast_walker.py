"""Tests for AST context extraction via tree-sitter walk-up."""

import pytest

from repo_surveyor.integration_concretiser.ast_walker import (
    extract_ast_context,
    FALLBACK_AST_CONTEXT,
)
from repo_surveyor.integration_patterns import Language


class TestExtractASTContextPython:
    """Walk-up logic for Python AST nodes."""

    PYTHON_SOURCE = b"""\
import requests

class MyService:
    def fetch_data(self, url):
        response = requests.get(url)
        return response.json()

    def handle_request(self):
        pass

def standalone_function():
    db.execute("SELECT * FROM users")
"""

    def test_walk_up_to_function_definition(self):
        ctx = extract_ast_context(self.PYTHON_SOURCE, Language.PYTHON, 5)
        assert ctx.node_type == "function_definition"
        assert "def fetch_data" in ctx.node_text
        assert ctx.start_line <= 5
        assert ctx.end_line >= 5

    def test_walk_up_to_standalone_function(self):
        ctx = extract_ast_context(self.PYTHON_SOURCE, Language.PYTHON, 12)
        assert ctx.node_type == "function_definition"
        assert "standalone_function" in ctx.node_text

    def test_import_line_returns_import_statement(self):
        ctx = extract_ast_context(self.PYTHON_SOURCE, Language.PYTHON, 1)
        assert "import" in ctx.node_type
        assert "requests" in ctx.node_text

    def test_line_range_covers_entire_function(self):
        ctx = extract_ast_context(self.PYTHON_SOURCE, Language.PYTHON, 5)
        assert ctx.start_line == 4
        assert ctx.end_line == 6


class TestExtractASTContextJava:
    """Walk-up logic for Java AST nodes."""

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

    def test_walk_up_to_method_declaration(self):
        ctx = extract_ast_context(self.JAVA_SOURCE, Language.JAVA, 10)
        assert "declaration" in ctx.node_type or "definition" in ctx.node_type
        assert "getUsers" in ctx.node_text

    def test_annotation_line_resolves_to_method(self):
        ctx = extract_ast_context(self.JAVA_SOURCE, Language.JAVA, 8)
        assert "GetMapping" in ctx.node_text or "getUsers" in ctx.node_text

    def test_class_level_context(self):
        ctx = extract_ast_context(self.JAVA_SOURCE, Language.JAVA, 6)
        assert "class" in ctx.node_type or "UserController" in ctx.node_text


class TestExtractASTContextTypeScript:
    """Walk-up logic for TypeScript AST nodes."""

    TS_SOURCE = b"""\
import express from 'express';

const app = express();

app.get('/api/users', async (req, res) => {
    const users = await db.query('SELECT * FROM users');
    res.json(users);
});

function processMessage(msg: Message): void {
    queue.publish('output', msg);
}
"""

    def test_walk_up_to_expression_statement(self):
        ctx = extract_ast_context(self.TS_SOURCE, Language.TYPESCRIPT, 6)
        assert ctx.node_text != ""
        assert ctx.start_line <= 6
        assert ctx.end_line >= 6

    def test_function_declaration(self):
        ctx = extract_ast_context(self.TS_SOURCE, Language.TYPESCRIPT, 11)
        assert "function" in ctx.node_type or "declaration" in ctx.node_type
        assert "processMessage" in ctx.node_text


class TestExtractASTContextEdgeCases:
    """Edge cases for AST context extraction."""

    def test_unsupported_language_returns_fallback(self):
        ctx = extract_ast_context(b"some content", Language.PLI, 1)
        assert ctx == FALLBACK_AST_CONTEXT

    def test_empty_file_returns_fallback(self):
        ctx = extract_ast_context(b"", Language.PYTHON, 1)
        assert ctx == FALLBACK_AST_CONTEXT

    def test_line_beyond_file_returns_fallback(self):
        ctx = extract_ast_context(b"x = 1\n", Language.PYTHON, 999)
        assert ctx == FALLBACK_AST_CONTEXT

    def test_context_has_positive_line_numbers(self):
        source = b"def foo():\n    pass\n"
        ctx = extract_ast_context(source, Language.PYTHON, 1)
        assert ctx.start_line >= 1
        assert ctx.end_line >= ctx.start_line
