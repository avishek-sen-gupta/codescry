"""Tests for dot_renderer module."""

from repo_surveyor.export.dot_renderer import GraphvizCliRenderer, NullDotRenderer


def test_null_renderer_returns_input_unchanged():
    renderer = NullDotRenderer()
    dot_source = 'digraph "test" { a -> b; }'
    assert renderer.render_svg(dot_source) == dot_source


def test_graphviz_cli_renderer_default_binary():
    renderer = GraphvizCliRenderer()
    assert renderer.dot_binary == "dot"


def test_graphviz_cli_renderer_custom_binary():
    renderer = GraphvizCliRenderer(dot_binary="/usr/local/bin/dot")
    assert renderer.dot_binary == "/usr/local/bin/dot"
