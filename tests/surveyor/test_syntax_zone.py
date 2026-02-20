"""Tests for tree-sitter syntax zone classification."""

import pytest

from repo_surveyor.integration_patterns import Language
from repo_surveyor.syntax_zone import (
    SyntaxRange,
    SyntaxRangeMap,
    SyntaxZone,
    build_syntax_range_map,
    classify_line,
    parse_file_zones,
)


class TestBuildSyntaxRangeMap:
    """Tests for building syntax range maps from tree-sitter parse trees."""

    def test_java_single_line_comment(self) -> None:
        """Should classify a Java single-line comment."""
        source = b"// this is a comment\nint x = 1;\n"
        range_map = parse_file_zones(source, Language.JAVA)
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.CODE

    def test_java_block_comment(self) -> None:
        """Should classify a Java multi-line block comment."""
        source = b"/* line one\n   line two\n   line three */\nint x = 1;\n"
        range_map = parse_file_zones(source, Language.JAVA)
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.COMMENT
        assert classify_line(range_map, 3) == SyntaxZone.COMMENT
        assert classify_line(range_map, 4) == SyntaxZone.CODE

    def test_java_string_literal(self) -> None:
        """Should classify a Java string literal line."""
        source = b'class Foo {\n  String s = "HttpClient.newHttpClient()";\n}\n'
        range_map = parse_file_zones(source, Language.JAVA)
        # The string is part of a variable declaration line, so it's mixed-zone
        # The string node doesn't span the full line, so it classifies as CODE
        assert classify_line(range_map, 2) == SyntaxZone.CODE

    def test_java_import_declaration(self) -> None:
        """Should classify a Java import as IMPORT zone."""
        source = b"import java.net.http.HttpClient;\n\nclass Foo {}\n"
        range_map = parse_file_zones(source, Language.JAVA)
        assert classify_line(range_map, 1) == SyntaxZone.IMPORT
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_python_single_line_comment(self) -> None:
        """Should classify a Python comment."""
        source = b"# Use requests for HTTP calls\nx = 1\n"
        range_map = parse_file_zones(source, Language.PYTHON)
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.CODE

    def test_python_docstring(self) -> None:
        """Should classify a Python docstring spanning multiple lines."""
        source = (
            b"def foo():\n"
            b'    """This is a docstring\n'
            b"    that spans multiple lines.\n"
            b'    """\n'
            b"    return 1\n"
        )
        range_map = parse_file_zones(source, Language.PYTHON)
        assert classify_line(range_map, 1) == SyntaxZone.CODE
        # Docstrings are parsed as string nodes — classified as STRING_LITERAL
        assert classify_line(range_map, 2) == SyntaxZone.STRING_LITERAL
        assert classify_line(range_map, 3) == SyntaxZone.STRING_LITERAL
        assert classify_line(range_map, 4) == SyntaxZone.STRING_LITERAL
        assert classify_line(range_map, 5) == SyntaxZone.CODE

    def test_python_import_statement(self) -> None:
        """Should classify Python imports."""
        source = b"import requests\nfrom flask import Flask\nx = 1\n"
        range_map = parse_file_zones(source, Language.PYTHON)
        assert classify_line(range_map, 1) == SyntaxZone.IMPORT
        assert classify_line(range_map, 2) == SyntaxZone.IMPORT
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_typescript_comment(self) -> None:
        """Should classify a TypeScript comment."""
        source = b"// fetch() is used for API calls\nconst x = 1;\n"
        range_map = parse_file_zones(source, Language.TYPESCRIPT)
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.CODE

    def test_go_comment(self) -> None:
        """Should classify a Go comment."""
        source = (
            b"// http.Get is the standard HTTP client\npackage main\nfunc main() {}\n"
        )
        range_map = parse_file_zones(source, Language.GO)
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.PACKAGE_DECLARATION
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_rust_comment_and_use(self) -> None:
        """Should classify a Rust comment and use declaration."""
        source = b"// use actix_web\nuse actix_web::HttpServer;\nfn main() {}\n"
        range_map = parse_file_zones(source, Language.RUST)
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.IMPORT
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_c_preproc_include(self) -> None:
        """Should classify a C #include as IMPORT zone."""
        source = b"#include <stdio.h>\n#include <stdlib.h>\nint main() { return 0; }\n"
        range_map = parse_file_zones(source, Language.C)
        assert classify_line(range_map, 1) == SyntaxZone.IMPORT
        assert classify_line(range_map, 2) == SyntaxZone.IMPORT
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_cpp_preproc_include(self) -> None:
        """Should classify a C++ #include as IMPORT zone."""
        source = b"#include <iostream>\n#include <vector>\nint main() { return 0; }\n"
        range_map = parse_file_zones(source, Language.CPP)
        assert classify_line(range_map, 1) == SyntaxZone.IMPORT
        assert classify_line(range_map, 2) == SyntaxZone.IMPORT
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_kotlin_import(self) -> None:
        """Should classify a Kotlin import as IMPORT zone."""
        source = b"import kotlin.io.println\nfun main() {}\n"
        range_map = parse_file_zones(source, Language.KOTLIN)
        assert classify_line(range_map, 1) == SyntaxZone.IMPORT
        assert classify_line(range_map, 2) == SyntaxZone.CODE

    def test_java_package_declaration(self) -> None:
        """Should classify a Java package declaration as PACKAGE_DECLARATION zone."""
        source = b"package com.example.grpc.service;\n\nclass Foo {}\n"
        range_map = parse_file_zones(source, Language.JAVA)
        assert classify_line(range_map, 1) == SyntaxZone.PACKAGE_DECLARATION
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_go_package_clause(self) -> None:
        """Should classify a Go package clause as PACKAGE_DECLARATION zone."""
        source = b"package main\n\nfunc main() {}\n"
        range_map = parse_file_zones(source, Language.GO)
        assert classify_line(range_map, 1) == SyntaxZone.PACKAGE_DECLARATION
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_kotlin_package_header(self) -> None:
        """Should classify a Kotlin package header as PACKAGE_DECLARATION zone."""
        source = b"package com.example.grpc\n\nfun main() {}\n"
        range_map = parse_file_zones(source, Language.KOTLIN)
        assert classify_line(range_map, 1) == SyntaxZone.PACKAGE_DECLARATION
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_scala_package_clause(self) -> None:
        """Should classify a Scala package clause as PACKAGE_DECLARATION zone."""
        source = b"package com.example.grpc\n\nobject Main {}\n"
        range_map = parse_file_zones(source, Language.SCALA)
        assert classify_line(range_map, 1) == SyntaxZone.PACKAGE_DECLARATION
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_csharp_file_scoped_namespace(self) -> None:
        """Should classify a C# file-scoped namespace as PACKAGE_DECLARATION zone."""
        source = b"namespace MyApp.Grpc.Services;\n\nclass Foo {}\n"
        range_map = parse_file_zones(source, Language.CSHARP)
        assert classify_line(range_map, 1) == SyntaxZone.PACKAGE_DECLARATION
        assert classify_line(range_map, 3) == SyntaxZone.CODE

    def test_php_namespace_definition(self) -> None:
        """Should classify a PHP namespace definition as PACKAGE_DECLARATION zone."""
        source = b"<?php\nnamespace App\\Grpc\\Services;\n\nclass Foo {}\n"
        range_map = parse_file_zones(source, Language.PHP)
        assert classify_line(range_map, 2) == SyntaxZone.PACKAGE_DECLARATION
        assert classify_line(range_map, 4) == SyntaxZone.CODE


class TestClassifyLine:
    """Tests for the classify_line function."""

    def test_empty_range_map_returns_code(self) -> None:
        """Every line classifies as CODE with an empty range map."""
        empty = SyntaxRangeMap(ranges=())
        assert classify_line(empty, 1) == SyntaxZone.CODE
        assert classify_line(empty, 100) == SyntaxZone.CODE

    def test_line_before_any_range(self) -> None:
        """Lines before all ranges classify as CODE."""
        range_map = SyntaxRangeMap(
            ranges=(SyntaxRange(start_row=5, end_row=5, zone=SyntaxZone.COMMENT),)
        )
        assert classify_line(range_map, 1) == SyntaxZone.CODE

    def test_line_after_all_ranges(self) -> None:
        """Lines after all ranges classify as CODE."""
        range_map = SyntaxRangeMap(
            ranges=(SyntaxRange(start_row=0, end_row=0, zone=SyntaxZone.COMMENT),)
        )
        assert classify_line(range_map, 5) == SyntaxZone.CODE

    def test_line_within_range(self) -> None:
        """Lines within a range get the range's zone."""
        range_map = SyntaxRangeMap(
            ranges=(SyntaxRange(start_row=2, end_row=4, zone=SyntaxZone.COMMENT),)
        )
        # 1-indexed line 3 = 0-indexed row 2
        assert classify_line(range_map, 3) == SyntaxZone.COMMENT
        assert classify_line(range_map, 4) == SyntaxZone.COMMENT
        assert classify_line(range_map, 5) == SyntaxZone.COMMENT

    def test_mixed_zone_line_code_then_comment(self) -> None:
        """A line with code followed by a comment classifies as CODE."""
        source = b"int x = 1; // HttpClient call\n"
        range_map = parse_file_zones(source, Language.JAVA)
        # The comment starts mid-line, so the line is not fully covered
        assert classify_line(range_map, 1) == SyntaxZone.CODE

    def test_multiple_ranges(self) -> None:
        """Should correctly classify lines across multiple ranges."""
        range_map = SyntaxRangeMap(
            ranges=(
                SyntaxRange(start_row=0, end_row=0, zone=SyntaxZone.COMMENT),
                SyntaxRange(start_row=2, end_row=2, zone=SyntaxZone.IMPORT),
                SyntaxRange(start_row=4, end_row=6, zone=SyntaxZone.STRING_LITERAL),
            )
        )
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.CODE
        assert classify_line(range_map, 3) == SyntaxZone.IMPORT
        assert classify_line(range_map, 4) == SyntaxZone.CODE
        assert classify_line(range_map, 5) == SyntaxZone.STRING_LITERAL
        assert classify_line(range_map, 6) == SyntaxZone.STRING_LITERAL
        assert classify_line(range_map, 7) == SyntaxZone.STRING_LITERAL


class TestParseFileZones:
    """Tests for parse_file_zones."""

    def test_unsupported_language_returns_empty_map(self) -> None:
        """PL/I has no tree-sitter support, should return empty range map."""
        content = b"DECLARE X FIXED;\n"
        range_map = parse_file_zones(content, Language.PLI)
        assert range_map.ranges == ()
        assert classify_line(range_map, 1) == SyntaxZone.CODE

    def test_empty_file_returns_empty_map(self) -> None:
        """An empty file should produce an empty range map."""
        range_map = parse_file_zones(b"", Language.JAVA)
        assert range_map.ranges == ()

    def test_all_code_file(self) -> None:
        """A file with only code lines produces no zone ranges."""
        source = b"int x = 1;\nint y = 2;\n"
        range_map = parse_file_zones(source, Language.JAVA)
        # There may be no comment/string/import ranges
        for line_num in range(1, 3):
            assert classify_line(range_map, line_num) == SyntaxZone.CODE

    def test_ranges_are_sorted(self) -> None:
        """Ranges should be sorted by start_row."""
        source = (
            b"import java.util.List;\n"
            b"// comment\n"
            b"class Foo {\n"
            b'    String s = "hello";\n'
            b"}\n"
        )
        range_map = parse_file_zones(source, Language.JAVA)
        rows = [r.start_row for r in range_map.ranges]
        assert rows == sorted(rows)


class TestMultiLineBlocks:
    """Tests for multi-line comment and string blocks."""

    def test_java_javadoc_comment(self) -> None:
        """Should classify a Javadoc comment spanning multiple lines."""
        source = (
            b"/**\n"
            b" * Use HttpClient for REST calls.\n"
            b" * @param url the URL\n"
            b" */\n"
            b"public void fetch(String url) {}\n"
        )
        range_map = parse_file_zones(source, Language.JAVA)
        assert classify_line(range_map, 1) == SyntaxZone.COMMENT
        assert classify_line(range_map, 2) == SyntaxZone.COMMENT
        assert classify_line(range_map, 3) == SyntaxZone.COMMENT
        assert classify_line(range_map, 4) == SyntaxZone.COMMENT
        assert classify_line(range_map, 5) == SyntaxZone.CODE

    def test_python_triple_quote_string(self) -> None:
        """Should classify a triple-quoted string as STRING_LITERAL."""
        source = (
            b"x = '''\n" b"import requests\n" b"requests.get(url)\n" b"'''\n" b"y = 1\n"
        )
        range_map = parse_file_zones(source, Language.PYTHON)
        # The triple-quote string spans lines 1-4, but line 1 has code too
        # Lines 2-3 are fully inside the string
        assert classify_line(range_map, 2) == SyntaxZone.STRING_LITERAL
        assert classify_line(range_map, 3) == SyntaxZone.STRING_LITERAL
        assert classify_line(range_map, 5) == SyntaxZone.CODE
