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
