"""Integration tests for CFG builder using real tree-sitter parsing."""

import pytest
from tree_sitter_language_pack import get_parser

from repo_surveyor.cfg_constructor.builder import build_cfg
from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec
from repo_surveyor.cfg_constructor.cfg_types import EdgeKind
from repo_surveyor.integration_patterns.types import Language


def _edges_of_kind(graph, kind):
    return [e for e in graph.edges if e.kind == kind]


def _node_by_type(graph, node_type):
    return [n for n in graph.nodes if n.node_type == node_type]


def _edge_set(graph):
    return {(e.source, e.target, e.kind) for e in graph.edges}


@pytest.fixture()
def java_spec(tmp_path):
    """Load the real Java CFG spec from the integration_patterns directory."""
    return get_cfg_spec(Language.JAVA)


@pytest.fixture()
def java_parser():
    return get_parser("java")


@pytest.fixture()
def python_spec():
    return get_cfg_spec(Language.PYTHON)


@pytest.fixture()
def python_parser():
    return get_parser("python")


@pytest.fixture()
def c_spec():
    return get_cfg_spec(Language.C)


@pytest.fixture()
def c_parser():
    return get_parser("c")


@pytest.fixture()
def js_spec():
    return get_cfg_spec(Language.JAVASCRIPT)


@pytest.fixture()
def js_parser():
    return get_parser("javascript")


class TestSimpleSequence:
    def test_java_two_statements(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        int x = 1;
        int y = 2;
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        assert graph.entry is not None
        assert graph.exit is not None
        # At least ENTRY, EXIT, and some statement nodes
        assert len(graph.nodes) >= 3
        # ENTRY -> something
        assert any(e.source == graph.entry for e in graph.edges)
        # something -> EXIT
        assert any(e.target == graph.exit for e in graph.edges)

    def test_python_simple_assignments(self, python_spec, python_parser):
        source = b"x = 1\ny = 2\n"
        graph = build_cfg(source, python_spec, python_parser)

        assert len(graph.nodes) >= 3
        assert any(e.source == graph.entry for e in graph.edges)
        assert any(e.target == graph.exit for e in graph.edges)


class TestIfElse:
    def test_java_if_else(self, java_spec, java_parser):
        source = b"""
class A {
    void m(int x) {
        if (x > 0) {
            int a = 1;
        } else {
            int b = 2;
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        true_edges = _edges_of_kind(graph, EdgeKind.TRUE)
        false_edges = _edges_of_kind(graph, EdgeKind.FALSE)
        assert len(true_edges) >= 1
        assert len(false_edges) >= 1

    def test_java_if_without_else(self, java_spec, java_parser):
        source = b"""
class A {
    void m(int x) {
        if (x > 0) {
            int a = 1;
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        true_edges = _edges_of_kind(graph, EdgeKind.TRUE)
        assert len(true_edges) >= 1
        # Still reaches EXIT
        assert any(e.target == graph.exit for e in graph.edges)


class TestWhileLoop:
    def test_java_while(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        int i = 0;
        while (i < 10) {
            i++;
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        true_edges = _edges_of_kind(graph, EdgeKind.TRUE)
        back_edges = _edges_of_kind(graph, EdgeKind.BACK)
        assert len(true_edges) >= 1
        assert len(back_edges) >= 1

    def test_java_while_with_break(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        while (true) {
            break;
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        # Should still reach EXIT (via break)
        assert any(e.target == graph.exit for e in graph.edges)

    def test_java_while_with_continue(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        int i = 0;
        while (i < 10) {
            i++;
            continue;
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        back_edges = _edges_of_kind(graph, EdgeKind.BACK)
        # At least 1 back edge from the loop body back, and 1 from continue
        assert len(back_edges) >= 1


class TestDoWhile:
    def test_java_do_while(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        int i = 0;
        do {
            i++;
        } while (i < 10);
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        back_edges = _edges_of_kind(graph, EdgeKind.BACK)
        assert len(back_edges) >= 1
        assert any(e.target == graph.exit for e in graph.edges)


class TestForLoop:
    def test_java_for(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        for (int i = 0; i < 10; i++) {
            int x = i;
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        true_edges = _edges_of_kind(graph, EdgeKind.TRUE)
        back_edges = _edges_of_kind(graph, EdgeKind.BACK)
        assert len(true_edges) >= 1
        assert len(back_edges) >= 1


class TestNestedIfInLoop:
    def test_java_nested(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        int i = 0;
        while (i < 10) {
            if (i % 2 == 0) {
                i += 2;
            } else {
                i++;
            }
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        true_edges = _edges_of_kind(graph, EdgeKind.TRUE)
        false_edges = _edges_of_kind(graph, EdgeKind.FALSE)
        back_edges = _edges_of_kind(graph, EdgeKind.BACK)
        assert len(true_edges) >= 2  # while TRUE + if TRUE
        assert len(false_edges) >= 1  # if FALSE
        assert len(back_edges) >= 1  # loop back


class TestTryCatch:
    def test_java_try_catch(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        try {
            int x = 1;
        } catch (Exception e) {
            int y = 2;
        }
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        assert any(e.target == graph.exit for e in graph.edges)
        assert len(graph.nodes) >= 3


class TestReturn:
    def test_java_return_in_middle(self, java_spec, java_parser):
        source = b"""
class A {
    int m(int x) {
        if (x < 0) {
            return -1;
        }
        return x;
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        # Both returns should reach EXIT
        return_nodes = _node_by_type(graph, "return_statement")
        assert len(return_nodes) >= 2
        # Each return should have an edge to EXIT
        return_ids = {n.id for n in return_nodes}
        exit_edges = [e for e in graph.edges if e.target == graph.exit]
        exit_sources = {e.source for e in exit_edges}
        assert return_ids & exit_sources  # at least one return reaches EXIT


class TestThrow:
    def test_java_throw(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
        throw new RuntimeException();
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        throw_nodes = _node_by_type(graph, "throw_statement")
        assert len(throw_nodes) >= 1
        # Throw should reach EXIT via EXCEPTION edge
        exception_edges = _edges_of_kind(graph, EdgeKind.EXCEPTION)
        assert len(exception_edges) >= 1


class TestEmptyBody:
    def test_java_empty_method(self, java_spec, java_parser):
        source = b"""
class A {
    void m() {
    }
}
"""
        graph = build_cfg(source, java_spec, java_parser)

        # Should still have ENTRY and EXIT
        assert graph.entry is not None
        assert graph.exit is not None


class TestSwitchNoFallthrough:
    def test_csharp_switch(self):
        """C# has no fallthrough."""
        spec = get_cfg_spec(Language.CSHARP)

        assert spec.switch_fallthrough is False


class TestSwitchFallthrough:
    def test_java_has_fallthrough(self, java_spec):
        assert java_spec.switch_fallthrough is True

    def test_c_has_fallthrough(self, c_spec):
        assert c_spec.switch_fallthrough is True

    def test_js_has_fallthrough(self, js_spec):
        assert js_spec.switch_fallthrough is True


class TestPythonSpecificPatterns:
    def test_python_for_loop(self, python_spec, python_parser):
        source = b"""
for i in range(10):
    x = i
"""
        graph = build_cfg(source, python_spec, python_parser)

        assert len(graph.nodes) >= 3
        assert any(e.target == graph.exit for e in graph.edges)

    def test_python_if_elif_else(self, python_spec, python_parser):
        source = b"""
if x > 0:
    a = 1
elif x < 0:
    b = 2
else:
    c = 3
"""
        graph = build_cfg(source, python_spec, python_parser)

        true_edges = _edges_of_kind(graph, EdgeKind.TRUE)
        false_edges = _edges_of_kind(graph, EdgeKind.FALSE)
        assert len(true_edges) >= 1
        assert len(false_edges) >= 1


class TestCPatterns:
    def test_c_for_loop(self, c_spec, c_parser):
        source = b"""
void f() {
    for (int i = 0; i < 10; i++) {
        int x = i;
    }
}
"""
        graph = build_cfg(source, c_spec, c_parser)

        back_edges = _edges_of_kind(graph, EdgeKind.BACK)
        assert len(back_edges) >= 1

    def test_c_switch(self, c_spec, c_parser):
        source = b"""
void f(int x) {
    switch (x) {
        case 1:
            break;
        case 2:
            break;
    }
}
"""
        graph = build_cfg(source, c_spec, c_parser)

        # C has fallthrough
        assert c_spec.switch_fallthrough is True
        assert any(e.target == graph.exit for e in graph.edges)


# ---------------------------------------------------------------------------
# Language fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def cpp_spec():
    return get_cfg_spec(Language.CPP)


@pytest.fixture()
def cpp_parser():
    return get_parser("cpp")


@pytest.fixture()
def csharp_spec():
    return get_cfg_spec(Language.CSHARP)


@pytest.fixture()
def csharp_parser():
    return get_parser("csharp")


@pytest.fixture()
def go_spec():
    return get_cfg_spec(Language.GO)


@pytest.fixture()
def go_parser():
    return get_parser("go")


@pytest.fixture()
def ruby_spec():
    return get_cfg_spec(Language.RUBY)


@pytest.fixture()
def ruby_parser():
    return get_parser("ruby")


@pytest.fixture()
def rust_spec():
    return get_cfg_spec(Language.RUST)


@pytest.fixture()
def rust_parser():
    return get_parser("rust")


@pytest.fixture()
def ts_spec():
    return get_cfg_spec(Language.TYPESCRIPT)


@pytest.fixture()
def ts_parser():
    return get_parser("typescript")


@pytest.fixture()
def kotlin_spec():
    return get_cfg_spec(Language.KOTLIN)


@pytest.fixture()
def kotlin_parser():
    return get_parser("kotlin")


@pytest.fixture()
def scala_spec():
    return get_cfg_spec(Language.SCALA)


@pytest.fixture()
def scala_parser():
    return get_parser("scala")


@pytest.fixture()
def php_spec():
    return get_cfg_spec(Language.PHP)


@pytest.fixture()
def php_parser():
    return get_parser("php")


@pytest.fixture()
def cobol_spec():
    return get_cfg_spec(Language.COBOL)


@pytest.fixture()
def cobol_parser():
    return get_parser("cobol")


# ---------------------------------------------------------------------------
# JavaScript tests
# ---------------------------------------------------------------------------


class TestJavaScriptPatterns:
    def test_js_if_else(self, js_spec, js_parser):
        source = b"""
function f(x) {
    if (x > 0) {
        let a = 1;
    } else {
        let b = 2;
    }
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_js_while_loop(self, js_spec, js_parser):
        source = b"""
function f() {
    let i = 0;
    while (i < 10) {
        i++;
    }
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_js_for_loop_with_break(self, js_spec, js_parser):
        source = b"""
function f() {
    for (let i = 0; i < 10; i++) {
        if (i === 5) break;
    }
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1
        assert any(e.target == graph.exit for e in graph.edges)

    def test_js_do_while(self, js_spec, js_parser):
        source = b"""
function f() {
    let i = 0;
    do {
        i++;
    } while (i < 10);
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1
        assert any(e.target == graph.exit for e in graph.edges)

    def test_js_try_catch_finally(self, js_spec, js_parser):
        source = b"""
function f() {
    try {
        let x = 1;
    } catch (e) {
        let y = 2;
    } finally {
        let z = 3;
    }
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        assert len(graph.nodes) >= 4
        assert any(e.target == graph.exit for e in graph.edges)

    def test_js_throw(self, js_spec, js_parser):
        source = b"""
function f() {
    throw new Error("fail");
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        assert len(_edges_of_kind(graph, EdgeKind.EXCEPTION)) >= 1

    def test_js_return(self, js_spec, js_parser):
        source = b"""
function f(x) {
    if (x) return 1;
    return 0;
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        return_nodes = _node_by_type(graph, "return_statement")
        assert len(return_nodes) >= 2

    def test_js_switch_fallthrough(self, js_spec, js_parser):
        source = b"""
function f(x) {
    switch (x) {
        case 1:
            let a = 1;
            break;
        case 2:
            let b = 2;
    }
}
"""
        graph = build_cfg(source, js_spec, js_parser)

        assert js_spec.switch_fallthrough is True
        assert any(e.target == graph.exit for e in graph.edges)


# ---------------------------------------------------------------------------
# TypeScript tests
# ---------------------------------------------------------------------------


class TestTypeScriptPatterns:
    def test_ts_if_else(self, ts_spec, ts_parser):
        source = b"""
function f(x: number): void {
    if (x > 0) {
        const a: number = 1;
    } else {
        const b: number = 2;
    }
}
"""
        graph = build_cfg(source, ts_spec, ts_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_ts_for_loop(self, ts_spec, ts_parser):
        source = b"""
function f(): void {
    for (let i = 0; i < 10; i++) {
        const x = i;
    }
}
"""
        graph = build_cfg(source, ts_spec, ts_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_ts_while_with_continue(self, ts_spec, ts_parser):
        source = b"""
function f(): void {
    let i = 0;
    while (i < 10) {
        i++;
        if (i === 5) continue;
    }
}
"""
        graph = build_cfg(source, ts_spec, ts_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_ts_try_catch(self, ts_spec, ts_parser):
        source = b"""
function f(): void {
    try {
        const x = 1;
    } catch (e) {
        const y = 2;
    }
}
"""
        graph = build_cfg(source, ts_spec, ts_parser)

        assert len(graph.nodes) >= 3
        assert any(e.target == graph.exit for e in graph.edges)

    def test_ts_switch_fallthrough(self, ts_spec):
        assert ts_spec.switch_fallthrough is True


# ---------------------------------------------------------------------------
# Go tests
# ---------------------------------------------------------------------------


class TestGoPatterns:
    def test_go_if_else(self, go_spec, go_parser):
        source = b"""
package main

func f(x int) {
    if x > 0 {
        y := 1
        _ = y
    } else {
        z := 2
        _ = z
    }
}
"""
        graph = build_cfg(source, go_spec, go_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_go_for_loop(self, go_spec, go_parser):
        source = b"""
package main

func f() {
    for i := 0; i < 10; i++ {
        x := i
        _ = x
    }
}
"""
        graph = build_cfg(source, go_spec, go_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_go_for_range(self, go_spec, go_parser):
        source = b"""
package main

func f() {
    items := []int{1, 2, 3}
    for _, v := range items {
        x := v
        _ = x
    }
}
"""
        graph = build_cfg(source, go_spec, go_parser)

        assert any(e.target == graph.exit for e in graph.edges)

    def test_go_switch(self, go_spec, go_parser):
        source = b"""
package main

func f(x int) {
    switch x {
    case 1:
        y := 1
        _ = y
    case 2:
        z := 2
        _ = z
    }
}
"""
        graph = build_cfg(source, go_spec, go_parser)

        assert go_spec.switch_fallthrough is False
        assert any(e.target == graph.exit for e in graph.edges)

    def test_go_return(self, go_spec, go_parser):
        source = b"""
package main

func f(x int) int {
    if x > 0 {
        return x
    }
    return -x
}
"""
        graph = build_cfg(source, go_spec, go_parser)

        return_nodes = _node_by_type(graph, "return_statement")
        assert len(return_nodes) >= 2

    def test_go_for_with_break(self, go_spec, go_parser):
        source = b"""
package main

func f() {
    for i := 0; i < 10; i++ {
        if i == 5 {
            break
        }
    }
}
"""
        graph = build_cfg(source, go_spec, go_parser)

        assert any(e.target == graph.exit for e in graph.edges)


# ---------------------------------------------------------------------------
# Ruby tests
# ---------------------------------------------------------------------------


class TestRubyPatterns:
    def test_ruby_if_else(self, ruby_spec, ruby_parser):
        source = b"""
def f(x)
  if x > 0
    a = 1
  else
    b = 2
  end
end
"""
        graph = build_cfg(source, ruby_spec, ruby_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_ruby_unless(self, ruby_spec, ruby_parser):
        source = b"""
def f(x)
  unless x > 0
    a = 1
  end
end
"""
        graph = build_cfg(source, ruby_spec, ruby_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1

    def test_ruby_while_loop(self, ruby_spec, ruby_parser):
        source = b"""
def f
  i = 0
  while i < 10
    i += 1
  end
end
"""
        graph = build_cfg(source, ruby_spec, ruby_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_ruby_until_loop(self, ruby_spec, ruby_parser):
        source = b"""
def f
  i = 0
  until i >= 10
    i += 1
  end
end
"""
        graph = build_cfg(source, ruby_spec, ruby_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_ruby_case_when(self, ruby_spec, ruby_parser):
        source = b"""
def f(x)
  case x
  when 1
    a = 1
  when 2
    b = 2
  end
end
"""
        graph = build_cfg(source, ruby_spec, ruby_parser)

        assert any(e.target == graph.exit for e in graph.edges)

    def test_ruby_begin_rescue(self, ruby_spec, ruby_parser):
        source = b"""
def f
  begin
    a = 1
  rescue StandardError => e
    b = 2
  end
end
"""
        graph = build_cfg(source, ruby_spec, ruby_parser)

        assert len(graph.nodes) >= 3
        assert any(e.target == graph.exit for e in graph.edges)

    def test_ruby_return(self, ruby_spec, ruby_parser):
        source = b"""
def f(x)
  return -1 if x < 0
  return x
end
"""
        graph = build_cfg(source, ruby_spec, ruby_parser)

        return_nodes = _node_by_type(graph, "return")
        assert len(return_nodes) >= 1


# ---------------------------------------------------------------------------
# Rust tests
# ---------------------------------------------------------------------------


class TestRustPatterns:
    def test_rust_if_else(self, rust_spec, rust_parser):
        source = b"""
fn f(x: i32) {
    if x > 0 {
        let a = 1;
    } else {
        let b = 2;
    }
}
"""
        graph = build_cfg(source, rust_spec, rust_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_rust_while_loop(self, rust_spec, rust_parser):
        source = b"""
fn f() {
    let mut i = 0;
    while i < 10 {
        i += 1;
    }
}
"""
        graph = build_cfg(source, rust_spec, rust_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_rust_loop_with_break(self, rust_spec, rust_parser):
        source = b"""
fn f() {
    let mut i = 0;
    loop {
        i += 1;
        if i > 5 {
            break;
        }
    }
}
"""
        graph = build_cfg(source, rust_spec, rust_parser)

        assert any(e.target == graph.exit for e in graph.edges)

    def test_rust_for_loop(self, rust_spec, rust_parser):
        source = b"""
fn f() {
    for i in 0..10 {
        let x = i;
    }
}
"""
        graph = build_cfg(source, rust_spec, rust_parser)

        assert any(e.target == graph.exit for e in graph.edges)

    def test_rust_match(self, rust_spec, rust_parser):
        source = b"""
fn f(x: i32) {
    match x {
        1 => { let a = 1; },
        2 => { let b = 2; },
        _ => { let c = 3; },
    }
}
"""
        graph = build_cfg(source, rust_spec, rust_parser)

        assert rust_spec.switch_fallthrough is False
        assert any(e.target == graph.exit for e in graph.edges)

    def test_rust_return(self, rust_spec, rust_parser):
        source = b"""
fn f(x: i32) -> i32 {
    if x < 0 {
        return -1;
    }
    return x;
}
"""
        graph = build_cfg(source, rust_spec, rust_parser)

        return_nodes = _node_by_type(graph, "return_expression")
        assert len(return_nodes) >= 2


# ---------------------------------------------------------------------------
# C++ tests
# ---------------------------------------------------------------------------


class TestCppPatterns:
    def test_cpp_if_else(self, cpp_spec, cpp_parser):
        source = b"""
void f(int x) {
    if (x > 0) {
        int a = 1;
    } else {
        int b = 2;
    }
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_cpp_for_loop(self, cpp_spec, cpp_parser):
        source = b"""
void f() {
    for (int i = 0; i < 10; i++) {
        int x = i;
    }
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_cpp_while_loop(self, cpp_spec, cpp_parser):
        source = b"""
void f() {
    int i = 0;
    while (i < 10) {
        i++;
    }
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_cpp_do_while(self, cpp_spec, cpp_parser):
        source = b"""
void f() {
    int i = 0;
    do {
        i++;
    } while (i < 10);
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_cpp_switch_fallthrough(self, cpp_spec, cpp_parser):
        source = b"""
void f(int x) {
    switch (x) {
        case 1:
            break;
        case 2:
            break;
    }
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        assert cpp_spec.switch_fallthrough is True
        assert any(e.target == graph.exit for e in graph.edges)

    def test_cpp_try_catch(self, cpp_spec, cpp_parser):
        source = b"""
void f() {
    try {
        int x = 1;
    } catch (int e) {
        int y = 2;
    }
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        assert len(graph.nodes) >= 3
        assert any(e.target == graph.exit for e in graph.edges)

    def test_cpp_throw(self, cpp_spec, cpp_parser):
        source = b"""
void f() {
    throw 42;
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.EXCEPTION)) >= 1

    def test_cpp_return(self, cpp_spec, cpp_parser):
        source = b"""
int f(int x) {
    if (x < 0) {
        return -1;
    }
    return x;
}
"""
        graph = build_cfg(source, cpp_spec, cpp_parser)

        return_nodes = _node_by_type(graph, "return_statement")
        assert len(return_nodes) >= 2


# ---------------------------------------------------------------------------
# C# tests
# ---------------------------------------------------------------------------


class TestCSharpPatterns:
    def test_csharp_if_else(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M(int x) {
        if (x > 0) {
            int a = 1;
        } else {
            int b = 2;
        }
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_csharp_for_loop(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M() {
        for (int i = 0; i < 10; i++) {
            int x = i;
        }
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_csharp_foreach(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M() {
        int[] items = {1, 2, 3};
        foreach (var item in items) {
            int x = item;
        }
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert any(e.target == graph.exit for e in graph.edges)

    def test_csharp_while_loop(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M() {
        int i = 0;
        while (i < 10) {
            i++;
        }
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_csharp_do_while(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M() {
        int i = 0;
        do {
            i++;
        } while (i < 10);
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_csharp_switch_no_fallthrough(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M(int x) {
        switch (x) {
            case 1:
                break;
            case 2:
                break;
        }
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert csharp_spec.switch_fallthrough is False
        assert any(e.target == graph.exit for e in graph.edges)

    def test_csharp_try_catch_finally(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M() {
        try {
            int x = 1;
        } catch (System.Exception e) {
            int y = 2;
        } finally {
            int z = 3;
        }
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert len(graph.nodes) >= 4
        assert any(e.target == graph.exit for e in graph.edges)

    def test_csharp_throw(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    void M() {
        throw new System.Exception();
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        assert len(_edges_of_kind(graph, EdgeKind.EXCEPTION)) >= 1

    def test_csharp_return(self, csharp_spec, csharp_parser):
        source = b"""
class A {
    int M(int x) {
        if (x < 0) {
            return -1;
        }
        return x;
    }
}
"""
        graph = build_cfg(source, csharp_spec, csharp_parser)

        return_nodes = _node_by_type(graph, "return_statement")
        assert len(return_nodes) >= 2


# ---------------------------------------------------------------------------
# Kotlin tests
# ---------------------------------------------------------------------------


class TestKotlinPatterns:
    def test_kotlin_if_else(self, kotlin_spec, kotlin_parser):
        source = b"""
fun f(x: Int) {
    if (x > 0) {
        val a = 1
    } else {
        val b = 2
    }
}
"""
        graph = build_cfg(source, kotlin_spec, kotlin_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_kotlin_while_loop(self, kotlin_spec, kotlin_parser):
        source = b"""
fun f() {
    var i = 0
    while (i < 10) {
        i++
    }
}
"""
        graph = build_cfg(source, kotlin_spec, kotlin_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_kotlin_do_while(self, kotlin_spec, kotlin_parser):
        source = b"""
fun f() {
    var i = 0
    do {
        i++
    } while (i < 10)
}
"""
        graph = build_cfg(source, kotlin_spec, kotlin_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_kotlin_for_loop(self, kotlin_spec, kotlin_parser):
        source = b"""
fun f() {
    for (i in 0..9) {
        val x = i
    }
}
"""
        graph = build_cfg(source, kotlin_spec, kotlin_parser)

        assert any(e.target == graph.exit for e in graph.edges)

    def test_kotlin_when(self, kotlin_spec, kotlin_parser):
        source = b"""
fun f(x: Int) {
    when (x) {
        1 -> { val a = 1 }
        2 -> { val b = 2 }
        else -> { val c = 3 }
    }
}
"""
        graph = build_cfg(source, kotlin_spec, kotlin_parser)

        assert kotlin_spec.switch_fallthrough is False
        assert any(e.target == graph.exit for e in graph.edges)

    def test_kotlin_try_catch(self, kotlin_spec, kotlin_parser):
        source = b"""
fun f() {
    try {
        val x = 1
    } catch (e: Exception) {
        val y = 2
    } finally {
        val z = 3
    }
}
"""
        graph = build_cfg(source, kotlin_spec, kotlin_parser)

        assert len(graph.nodes) >= 4
        assert any(e.target == graph.exit for e in graph.edges)

    def test_kotlin_return(self, kotlin_spec, kotlin_parser):
        source = b"""
fun f(x: Int): Int {
    if (x < 0) {
        return -1
    }
    return x
}
"""
        graph = build_cfg(source, kotlin_spec, kotlin_parser)

        return_nodes = _node_by_type(graph, "jump_expression")
        assert len(return_nodes) >= 2


# ---------------------------------------------------------------------------
# Scala tests
# ---------------------------------------------------------------------------


class TestScalaPatterns:
    def test_scala_if_else(self, scala_spec, scala_parser):
        source = b"""
object A {
  def f(x: Int): Unit = {
    if (x > 0) {
      val a = 1
    } else {
      val b = 2
    }
  }
}
"""
        graph = build_cfg(source, scala_spec, scala_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_scala_while_loop(self, scala_spec, scala_parser):
        source = b"""
object A {
  def f(): Unit = {
    var i = 0
    while (i < 10) {
      i += 1
    }
  }
}
"""
        graph = build_cfg(source, scala_spec, scala_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_scala_match(self, scala_spec, scala_parser):
        source = b"""
object A {
  def f(x: Int): Unit = {
    x match {
      case 1 => val a = 1
      case 2 => val b = 2
      case _ => val c = 3
    }
  }
}
"""
        graph = build_cfg(source, scala_spec, scala_parser)

        assert scala_spec.switch_fallthrough is False
        assert any(e.target == graph.exit for e in graph.edges)

    def test_scala_try_catch(self, scala_spec, scala_parser):
        source = b"""
object A {
  def f(): Unit = {
    try {
      val x = 1
    } catch {
      case e: Exception => val y = 2
    } finally {
      val z = 3
    }
  }
}
"""
        graph = build_cfg(source, scala_spec, scala_parser)

        assert len(graph.nodes) >= 4
        assert any(e.target == graph.exit for e in graph.edges)

    def test_scala_return(self, scala_spec, scala_parser):
        source = b"""
object A {
  def f(x: Int): Int = {
    if (x < 0) {
      return -1
    }
    return x
  }
}
"""
        graph = build_cfg(source, scala_spec, scala_parser)

        return_nodes = _node_by_type(graph, "return_expression")
        assert len(return_nodes) >= 2


# ---------------------------------------------------------------------------
# PHP tests
# ---------------------------------------------------------------------------


class TestPhpPatterns:
    def test_php_if_else(self, php_spec, php_parser):
        source = b"""<?php
function f($x) {
    if ($x > 0) {
        $a = 1;
    } else {
        $b = 2;
    }
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.FALSE)) >= 1

    def test_php_while_loop(self, php_spec, php_parser):
        source = b"""<?php
function f() {
    $i = 0;
    while ($i < 10) {
        $i++;
    }
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert len(_edges_of_kind(graph, EdgeKind.TRUE)) >= 1
        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_php_for_loop(self, php_spec, php_parser):
        source = b"""<?php
function f() {
    for ($i = 0; $i < 10; $i++) {
        $x = $i;
    }
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_php_foreach(self, php_spec, php_parser):
        source = b"""<?php
function f() {
    $items = [1, 2, 3];
    foreach ($items as $item) {
        $x = $item;
    }
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert any(e.target == graph.exit for e in graph.edges)

    def test_php_do_while(self, php_spec, php_parser):
        source = b"""<?php
function f() {
    $i = 0;
    do {
        $i++;
    } while ($i < 10);
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert len(_edges_of_kind(graph, EdgeKind.BACK)) >= 1

    def test_php_switch_fallthrough(self, php_spec, php_parser):
        source = b"""<?php
function f($x) {
    switch ($x) {
        case 1:
            $a = 1;
            break;
        case 2:
            $b = 2;
            break;
    }
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert php_spec.switch_fallthrough is True
        assert any(e.target == graph.exit for e in graph.edges)

    def test_php_try_catch_finally(self, php_spec, php_parser):
        source = b"""<?php
function f() {
    try {
        $x = 1;
    } catch (Exception $e) {
        $y = 2;
    } finally {
        $z = 3;
    }
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert len(graph.nodes) >= 4
        assert any(e.target == graph.exit for e in graph.edges)

    def test_php_throw(self, php_spec, php_parser):
        source = b"""<?php
function f() {
    throw new Exception("fail");
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        assert len(_edges_of_kind(graph, EdgeKind.EXCEPTION)) >= 1

    def test_php_return(self, php_spec, php_parser):
        source = b"""<?php
function f($x) {
    if ($x < 0) {
        return -1;
    }
    return $x;
}
"""
        graph = build_cfg(source, php_spec, php_parser)

        return_nodes = _node_by_type(graph, "return_statement")
        assert len(return_nodes) >= 2


# ---------------------------------------------------------------------------
# COBOL tests
# ---------------------------------------------------------------------------


class TestCobolPatterns:
    def test_cobol_if_else(self, cobol_spec, cobol_parser):
        source = b"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.
       PROCEDURE DIVISION.
           IF X > 0
               DISPLAY "POSITIVE"
           ELSE
               DISPLAY "NON-POSITIVE"
           END-IF.
           STOP RUN.
"""
        graph = build_cfg(source, cobol_spec, cobol_parser)

        assert len(graph.nodes) >= 3
        assert any(e.target == graph.exit for e in graph.edges)

    def test_cobol_evaluate(self, cobol_spec, cobol_parser):
        source = b"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.
       PROCEDURE DIVISION.
           EVALUATE X
               WHEN 1
                   DISPLAY "ONE"
               WHEN 2
                   DISPLAY "TWO"
               WHEN OTHER
                   DISPLAY "OTHER"
           END-EVALUATE.
           STOP RUN.
"""
        graph = build_cfg(source, cobol_spec, cobol_parser)

        assert len(graph.nodes) >= 3
        assert any(e.target == graph.exit for e in graph.edges)

    def test_cobol_goback(self, cobol_spec, cobol_parser):
        source = b"""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.
       PROCEDURE DIVISION.
           DISPLAY "HELLO".
           GOBACK.
"""
        graph = build_cfg(source, cobol_spec, cobol_parser)

        assert len(graph.nodes) >= 3
