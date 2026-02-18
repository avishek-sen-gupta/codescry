#!/usr/bin/env python3
"""Generate example SVG diagrams from Codescry's own codebase.

Produces:
  - docs/images/cfg_example.svg — CFG for ``_dep_matches_pattern``
  - docs/images/integration_signals.svg — integration signal diagram

Requires: Graphviz ``dot`` binary on PATH.
"""

from pathlib import Path

from tree_sitter_language_pack import get_parser

from repo_surveyor.cfg_constructor.builder import build_cfg
from repo_surveyor.cfg_constructor.cfg_role_registry import get_cfg_spec
from repo_surveyor.export import (
    GraphvizCliRenderer,
    cfg_to_dot,
    integration_to_dot,
)
from repo_surveyor.ctags import CTagsConfig, run_ctags
from repo_surveyor.integration_detector import detect_integrations
from repo_surveyor.integration_patterns.types import Language
from repo_surveyor.symbol_resolver import resolve_integration_signals

REPO_ROOT = Path(__file__).resolve().parent.parent
OUTPUT_DIR = REPO_ROOT / "docs" / "images"

# Target file and function for CFG diagram
CFG_TARGET = REPO_ROOT / "src" / "repo_surveyor" / "package_parsers" / "__init__.py"
CFG_FUNCTION_NAME = "_dep_matches_pattern"


def _extract_function_body(source: bytes, function_name: str) -> bytes:
    """Extract the body source bytes of a named function using tree-sitter."""
    parser = get_parser("python")
    tree = parser.parse(source)

    for node in _walk(tree.root_node):
        if node.type == "function_definition":
            name_node = node.child_by_field_name("name")
            if name_node is not None and name_node.text.decode() == function_name:
                body_node = node.child_by_field_name("body")
                if body_node is not None:
                    return source[body_node.start_byte : body_node.end_byte]
                return source[node.start_byte : node.end_byte]

    raise ValueError(f"Function {function_name!r} not found")


def _walk(node):
    """Depth-first walk of a tree-sitter node."""
    yield node
    for child in node.children:
        yield from _walk(child)


def generate_cfg_diagram(renderer: GraphvizCliRenderer) -> None:
    """Generate the CFG example SVG."""
    source = CFG_TARGET.read_bytes()
    func_body = _extract_function_body(source, CFG_FUNCTION_NAME)
    lang_spec = get_cfg_spec(Language.PYTHON)
    parser = get_parser("python")

    cfg = build_cfg(func_body, lang_spec, parser)

    dot_source = cfg_to_dot(cfg, title=f"CFG: {CFG_FUNCTION_NAME}")
    svg = renderer.render_svg(dot_source)

    out_path = OUTPUT_DIR / "cfg_example.svg"
    out_path.write_text(svg)
    print(f"Wrote {out_path}")


def generate_integration_diagram(renderer: GraphvizCliRenderer) -> None:
    """Generate the integration signals example SVG."""
    integration_result = detect_integrations(
        repo_path=REPO_ROOT,
        languages=[Language.PYTHON],
    )

    ctags_result = run_ctags(REPO_ROOT, CTagsConfig(languages=["Python"]))

    resolution = resolve_integration_signals(
        ctags_result=ctags_result,
        integration_result=integration_result,
        repo_path=str(REPO_ROOT),
    )

    dot_source = integration_to_dot(
        resolution,
        title="Codescry Integration Signals",
        max_profiles=15,
    )
    svg = renderer.render_svg(dot_source)

    out_path = OUTPUT_DIR / "integration_signals.svg"
    out_path.write_text(svg)
    print(f"Wrote {out_path}")


def main() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    renderer = GraphvizCliRenderer()

    print("Generating CFG diagram...")
    generate_cfg_diagram(renderer)

    print("Generating integration signals diagram...")
    generate_integration_diagram(renderer)

    print("Done.")


if __name__ == "__main__":
    main()
