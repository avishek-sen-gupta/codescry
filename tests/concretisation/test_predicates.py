"""Tests for individual evidence predicate functions."""

import pytest

from repo_surveyor.integration_concretiser.predicate_context_builder import (
    PredicateContext,
)
from repo_surveyor.integration_concretiser.predicates import (
    enclosing_function_calls,
    in_assertion,
    in_config_dir,
    in_constant_decl,
    in_generated_file,
    in_log_statement,
    in_string_context,
    in_test_file,
    in_vendor_dir,
    line_matches,
    path_matches,
    sibling_line_matches,
    PREDICATE_DISPATCH,
)
from repo_surveyor.integration_concretiser.evidence_predicates import PredicateName
from repo_surveyor.integration_patterns import Language


def _ctx(
    file_path: str = "src/main/App.java",
    line_number: int = 10,
    line_content: str = "conn.open()",
    language: Language | None = Language.JAVA,
    source_lines: tuple[str, ...] = (),
    enclosing_node_text: str = "",
    enclosing_node_type: str = "method_declaration",
    ancestor_node_types: tuple[str, ...] = (),
) -> PredicateContext:
    return PredicateContext(
        file_path=file_path,
        line_number=line_number,
        line_content=line_content,
        language=language,
        source_lines=source_lines if source_lines else (line_content,),
        enclosing_node_text=(
            enclosing_node_text if enclosing_node_text else line_content
        ),
        enclosing_node_type=enclosing_node_type,
        ancestor_node_types=ancestor_node_types,
    )


# ---------------------------------------------------------------------------
# Structural predicates
# ---------------------------------------------------------------------------


class TestInStringContext:
    def test_string_literal_ancestor_fires(self):
        ctx = _ctx(
            ancestor_node_types=("identifier", "string_literal", "expression_statement")
        )
        assert in_string_context(ctx, "") is True

    def test_string_ancestor_fires(self):
        ctx = _ctx(ancestor_node_types=("identifier", "string", "call_expression"))
        assert in_string_context(ctx, "") is True

    def test_no_string_ancestor_does_not_fire(self):
        ctx = _ctx(
            ancestor_node_types=("identifier", "call_expression", "method_declaration")
        )
        assert in_string_context(ctx, "") is False

    def test_empty_ancestors_does_not_fire(self):
        ctx = _ctx(ancestor_node_types=())
        assert in_string_context(ctx, "") is False

    def test_template_string_fires(self):
        ctx = _ctx(ancestor_node_types=("identifier", "template_string"))
        assert in_string_context(ctx, "") is True


class TestInConstantDecl:
    def test_java_static_final(self):
        ctx = _ctx(
            enclosing_node_text='private static final String URL = "http://api.example.com";'
        )
        assert in_constant_decl(ctx, "") is True

    def test_js_const(self):
        ctx = _ctx(enclosing_node_text='const API_URL = "http://api.example.com";')
        assert in_constant_decl(ctx, "") is True

    def test_kotlin_val(self):
        ctx = _ctx(enclosing_node_text='val CONNECTION_STRING = "jdbc:mysql://..."')
        assert in_constant_decl(ctx, "") is True

    def test_python_allcaps(self):
        ctx = _ctx(enclosing_node_text='DATABASE_URL = "postgres://..."')
        assert in_constant_decl(ctx, "") is True

    def test_rust_const_item_node_type(self):
        ctx = _ctx(
            enclosing_node_type="const_item",
            enclosing_node_text="const MAX: u32 = 100;",
        )
        assert in_constant_decl(ctx, "") is True

    def test_regular_assignment_does_not_fire(self):
        ctx = _ctx(enclosing_node_text="response = requests.get(url)")
        assert in_constant_decl(ctx, "") is False


class TestInAssertion:
    def test_python_assert(self):
        ctx = _ctx(enclosing_node_text="assert response.status_code == 200")
        assert in_assertion(ctx, "") is True

    def test_junit_assertEquals(self):
        ctx = _ctx(enclosing_node_text='assertEquals("expected", actual)')
        assert in_assertion(ctx, "") is True

    def test_jest_expect(self):
        ctx = _ctx(enclosing_node_text="expect(fetch).toHaveBeenCalled()")
        assert in_assertion(ctx, "") is True

    def test_rspec_should(self):
        ctx = _ctx(enclosing_node_text='response.body.should include("OK")')
        assert in_assertion(ctx, "") is True

    def test_rust_assert_eq(self):
        ctx = _ctx(enclosing_node_text="assert_eq!(result, expected)")
        assert in_assertion(ctx, "") is True

    def test_regular_code_does_not_fire(self):
        ctx = _ctx(enclosing_node_text="response = client.get(url)")
        assert in_assertion(ctx, "") is False


# ---------------------------------------------------------------------------
# File/path predicates
# ---------------------------------------------------------------------------


class TestInTestFile:
    def test_python_test_prefix(self):
        ctx = _ctx(file_path="tests/test_api_client.py")
        assert in_test_file(ctx, "") is True

    def test_python_test_suffix(self):
        ctx = _ctx(file_path="src/api_client_test.py")
        assert in_test_file(ctx, "") is True

    def test_java_test_suffix(self):
        ctx = _ctx(file_path="src/test/java/ApiClientTest.java")
        assert in_test_file(ctx, "") is True

    def test_go_test_suffix(self):
        ctx = _ctx(file_path="pkg/client/client_test.go")
        assert in_test_file(ctx, "") is True

    def test_js_spec_file(self):
        ctx = _ctx(file_path="src/components/App.spec.tsx")
        assert in_test_file(ctx, "") is True

    def test_js_test_file(self):
        ctx = _ctx(file_path="src/utils/api.test.js")
        assert in_test_file(ctx, "") is True

    def test_ruby_spec(self):
        ctx = _ctx(file_path="spec/models/user_spec.rb")
        assert in_test_file(ctx, "") is True

    def test_tests_directory(self):
        ctx = _ctx(file_path="project/tests/integration/test_db.py")
        assert in_test_file(ctx, "") is True

    def test_jest_tests_dir(self):
        ctx = _ctx(file_path="src/__tests__/App.tsx")
        assert in_test_file(ctx, "") is True

    def test_conftest(self):
        ctx = _ctx(file_path="tests/conftest.py")
        assert in_test_file(ctx, "") is True

    def test_production_code_does_not_fire(self):
        ctx = _ctx(file_path="src/main/java/ApiClient.java")
        assert in_test_file(ctx, "") is False


class TestInVendorDir:
    def test_vendor_directory(self):
        ctx = _ctx(file_path="vendor/github.com/pkg/errors/errors.go")
        assert in_vendor_dir(ctx, "") is True

    def test_node_modules(self):
        ctx = _ctx(file_path="node_modules/express/lib/router.js")
        assert in_vendor_dir(ctx, "") is True

    def test_third_party(self):
        ctx = _ctx(file_path="third_party/proto/api.proto")
        assert in_vendor_dir(ctx, "") is True

    def test_build_generated(self):
        ctx = _ctx(file_path="build/generated/source/proto/main/java/Api.java")
        assert in_vendor_dir(ctx, "") is True

    def test_regular_src_does_not_fire(self):
        ctx = _ctx(file_path="src/main/java/Api.java")
        assert in_vendor_dir(ctx, "") is False


class TestInConfigDir:
    def test_config_directory(self):
        ctx = _ctx(file_path="config/database.yml")
        assert in_config_dir(ctx, "") is True

    def test_yaml_file(self):
        ctx = _ctx(file_path="app/settings.yaml")
        assert in_config_dir(ctx, "") is True

    def test_env_file(self):
        ctx = _ctx(file_path=".env.production")
        assert in_config_dir(ctx, "") is True

    def test_toml_file(self):
        ctx = _ctx(file_path="pyproject.toml")
        assert in_config_dir(ctx, "") is True

    def test_properties_file(self):
        ctx = _ctx(file_path="application.properties")
        assert in_config_dir(ctx, "") is True

    def test_source_code_does_not_fire(self):
        ctx = _ctx(file_path="src/main/App.java")
        assert in_config_dir(ctx, "") is False


class TestInGeneratedFile:
    def test_generated_directory(self):
        ctx = _ctx(file_path="src/generated/api_pb2.py")
        assert in_generated_file(ctx, "") is True

    def test_pb2_suffix(self):
        ctx = _ctx(file_path="src/proto/api_pb2.py")
        assert in_generated_file(ctx, "") is True

    def test_pb_go_suffix(self):
        ctx = _ctx(file_path="internal/api.pb.go")
        assert in_generated_file(ctx, "") is True

    def test_header_comment(self):
        ctx = _ctx(
            file_path="src/Api.java",
            source_lines=(
                "// Auto-generated by protobuf compiler",
                "// Do not edit",
                "package com.example;",
            ),
        )
        assert in_generated_file(ctx, "") is True

    def test_regular_file_does_not_fire(self):
        ctx = _ctx(
            file_path="src/main/App.java", source_lines=("package com.example;",)
        )
        assert in_generated_file(ctx, "") is False


class TestPathMatches:
    def test_matches_regex(self):
        ctx = _ctx(file_path="src/adapters/http/client.py")
        assert path_matches(ctx, r"/adapters/") is True

    def test_no_match(self):
        ctx = _ctx(file_path="src/main/App.java")
        assert path_matches(ctx, r"/adapters/") is False

    def test_empty_pattern_does_not_fire(self):
        ctx = _ctx(file_path="anything")
        assert path_matches(ctx, "") is False


# ---------------------------------------------------------------------------
# Textual predicates
# ---------------------------------------------------------------------------


class TestInLogStatement:
    def test_java_logger(self):
        ctx = _ctx(enclosing_node_text='logger.info("Connecting to {}", url)')
        assert in_log_statement(ctx, "") is True

    def test_python_logging(self):
        ctx = _ctx(enclosing_node_text='logging.warning("Connection refused")')
        assert in_log_statement(ctx, "") is True

    def test_js_console(self):
        ctx = _ctx(enclosing_node_text='console.log("API response:", data)')
        assert in_log_statement(ctx, "") is True

    def test_go_log(self):
        ctx = _ctx(enclosing_node_text='log.Printf("Request to %s", url)')
        assert in_log_statement(ctx, "") is True

    def test_rust_log_macro(self):
        ctx = _ctx(enclosing_node_text='log::info!("Connected to {}", addr)')
        assert in_log_statement(ctx, "") is True

    def test_python_print(self):
        ctx = _ctx(enclosing_node_text='print("connecting to", url)')
        assert in_log_statement(ctx, "") is True

    def test_java_system_out(self):
        ctx = _ctx(enclosing_node_text='System.out.println("Connected")')
        assert in_log_statement(ctx, "") is True

    def test_regular_call_does_not_fire(self):
        ctx = _ctx(enclosing_node_text="response = requests.get(url)")
        assert in_log_statement(ctx, "") is False


class TestEnclosingFunctionCalls:
    def test_matches_pattern(self):
        ctx = _ctx(enclosing_node_text="response = requests.get(url)")
        assert enclosing_function_calls(ctx, r"requests\.\w+") is True

    def test_no_match(self):
        ctx = _ctx(enclosing_node_text="result = compute(x)")
        assert enclosing_function_calls(ctx, r"requests\.\w+") is False

    def test_empty_pattern_does_not_fire(self):
        ctx = _ctx(enclosing_node_text="anything")
        assert enclosing_function_calls(ctx, "") is False


class TestSiblingLineMatches:
    def test_sibling_within_window(self):
        lines = (
            "import os",
            "import socket",
            "",
            "def connect():",
            "    sock = socket.socket()",
            "    sock.connect(('localhost', 8080))",  # signal line (6, 1-indexed)
            "    sock.send(data)",
            "    response = sock.recv(1024)",
            "",
        )
        ctx = _ctx(line_number=6, source_lines=lines)
        assert sibling_line_matches(ctx, r"socket\.socket") is True

    def test_no_match_outside_window(self):
        lines = tuple(f"line {i}" for i in range(20))
        ctx = _ctx(line_number=15, source_lines=lines)
        # Line 1 is far outside the +/-5 window of line 15
        assert sibling_line_matches(ctx, r"line 1$") is False

    def test_empty_pattern_does_not_fire(self):
        ctx = _ctx(source_lines=("anything",))
        assert sibling_line_matches(ctx, "") is False


class TestLineMatches:
    def test_matches(self):
        ctx = _ctx(
            line_content='conn = DriverManager.getConnection("jdbc:mysql://...")'
        )
        assert line_matches(ctx, r"jdbc:") is True

    def test_no_match(self):
        ctx = _ctx(line_content="result = compute(x)")
        assert line_matches(ctx, r"jdbc:") is False

    def test_empty_pattern_does_not_fire(self):
        ctx = _ctx(line_content="anything")
        assert line_matches(ctx, "") is False


# ---------------------------------------------------------------------------
# Dispatch table completeness
# ---------------------------------------------------------------------------


class TestDispatchTable:
    def test_all_predicate_names_have_dispatch(self):
        for name in PredicateName:
            assert name in PREDICATE_DISPATCH, f"Missing dispatch for {name}"

    def test_dispatch_table_has_no_extra_entries(self):
        for name in PREDICATE_DISPATCH:
            assert name in PredicateName, f"Extra dispatch entry {name}"
