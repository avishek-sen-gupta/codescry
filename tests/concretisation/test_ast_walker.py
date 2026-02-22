"""Tests for AST context extraction via tree-sitter walk-up."""

import pytest

from repo_surveyor.integration_concretiser.ast_walker import (
    extract_ast_context,
    extract_statement_context,
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


class TestExtractStatementContextRust:
    """Rust uses _expression suffix for control flow — suffix heuristics miss these."""

    RUST_SOURCE = b"""\
fn handle_request(req: Request) -> Response {
    if req.is_valid() {
        let result = match req.method {
            Method::GET => get_handler(req),
            Method::POST => post_handler(req),
        };
        return result;
    }
    Response::not_found()
}
"""

    def test_if_expression_detected_as_statement(self):
        ctx = extract_statement_context(self.RUST_SOURCE, Language.RUST, 2)
        assert ctx != FALLBACK_AST_CONTEXT
        assert ctx.node_type in ("if_expression", "function_item")
        assert "if" in ctx.node_text

    def test_match_expression_detected_as_statement(self):
        ctx = extract_statement_context(self.RUST_SOURCE, Language.RUST, 3)
        assert ctx != FALLBACK_AST_CONTEXT
        assert "match" in ctx.node_text or "let" in ctx.node_text

    def test_return_expression_detected_as_statement(self):
        ctx = extract_statement_context(self.RUST_SOURCE, Language.RUST, 7)
        assert ctx != FALLBACK_AST_CONTEXT
        assert "return" in ctx.node_text


class TestExtractStatementContextRuby:
    """Ruby uses bare keywords (if, case, call) — no _statement suffix."""

    RUBY_SOURCE = b"""\
class UserController < ApplicationController
  def index
    if params[:active]
      @users = User.where(active: true)
    end
    render json: @users
  end

  def show
    case params[:format]
    when "json"
      render json: @user
    when "xml"
      render xml: @user
    end
  end
end
"""

    def test_bare_if_detected_as_statement(self):
        ctx = extract_statement_context(self.RUBY_SOURCE, Language.RUBY, 3)
        assert ctx != FALLBACK_AST_CONTEXT
        assert "if" in ctx.node_type or "if" in ctx.node_text

    def test_case_detected_as_statement(self):
        ctx = extract_statement_context(self.RUBY_SOURCE, Language.RUBY, 10)
        assert ctx != FALLBACK_AST_CONTEXT
        assert "case" in ctx.node_type or "case" in ctx.node_text

    def test_call_detected_as_statement(self):
        ctx = extract_statement_context(self.RUBY_SOURCE, Language.RUBY, 6)
        assert ctx != FALLBACK_AST_CONTEXT
        assert "render" in ctx.node_text or "call" in ctx.node_type


class TestExtractStatementContextKotlin:
    """Kotlin uses _expression suffix for when/if — suffix heuristics miss these."""

    KOTLIN_SOURCE = b"""\
fun classify(status: Int): String {
    return when (status) {
        200 -> "OK"
        404 -> "Not Found"
        else -> "Unknown"
    }
}
"""

    def test_when_expression_detected_as_statement(self):
        ctx = extract_statement_context(self.KOTLIN_SOURCE, Language.KOTLIN, 2)
        assert ctx != FALLBACK_AST_CONTEXT
        assert "when" in ctx.node_text or "return" in ctx.node_text


class TestExtractStatementContextCOBOL:
    """COBOL uses _header suffix — suffix heuristics miss these."""

    COBOL_SOURCE = b"""\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
           IF WS-STATUS = 'OK'
               DISPLAY 'Success'
           ELSE
               DISPLAY 'Failure'
           END-IF.
           STOP RUN.
"""

    def test_if_header_detected_as_statement(self):
        ctx = extract_statement_context(self.COBOL_SOURCE, Language.COBOL, 4)
        assert ctx != FALLBACK_AST_CONTEXT
        assert ctx.start_line <= 4
        assert ctx.end_line >= 4
