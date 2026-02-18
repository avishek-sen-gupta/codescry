"""Convert a ResolutionResult to Graphviz DOT format.

Pure functions — no I/O, no side effects.
"""

from repo_surveyor.symbol_resolver import ResolutionResult, SymbolIntegrationProfile


_CONFIDENCE_STYLE: dict[str, str] = {
    "high": 'style=bold, color="#0d9488"',
    "medium": 'color="#f59e0b"',
    "low": 'style=dashed, color="#94a3b8"',
}

_DEFAULT_EDGE_STYLE = 'color="#64748b"'


def _escape_dot(text: str) -> str:
    """Escape text for DOT label strings."""
    return text.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")


def _integration_node_id(integration_type_value: str) -> str:
    """Stable DOT node id for an integration type."""
    return f"int_{integration_type_value}"


def _symbol_node_id(profile: SymbolIntegrationProfile) -> str:
    """Stable DOT node id for a symbol profile."""
    safe = (
        profile.symbol_id.replace("/", "_")
        .replace(":", "_")
        .replace(".", "_")
        .replace("-", "_")
    )
    return f"sym_{safe}"


def integration_to_dot(
    result: ResolutionResult,
    title: str = "Integration Signals",
    max_profiles: int = 15,
) -> str:
    """Convert a ResolutionResult to a Graphviz DOT string.

    Produces a ``graph`` (undirected) suitable for force-directed layout
    engines like ``fdp`` or ``neato``.  Symbol nodes are styled as rounded
    boxes grouped by file path; integration-type nodes are coloured ellipses.

    Args:
        result: The resolution result containing symbol profiles.
        title: Graph title.
        max_profiles: Maximum number of symbol profiles to include.

    Returns:
        A complete DOT ``graph`` string.
    """
    profiles = result.profiles[:max_profiles]

    lines = [
        f'graph "{_escape_dot(title)}" {{',
        "  overlap=false;",
        "  splines=true;",
        "  K=0.6;",
        '  sep="+4";',
        f'  label="{_escape_dot(title)}";',
        '  labelloc="t";',
        '  fontsize=16; fontname="Helvetica";',
        '  node [fontsize=10, fontname="Helvetica"];',
        '  edge [fontsize=8, fontname="Helvetica"];',
        "",
    ]

    # Group profiles by file path for clustering
    by_path: dict[str, list[SymbolIntegrationProfile]] = {}
    for profile in profiles:
        by_path.setdefault(profile.symbol_path, []).append(profile)

    for cluster_idx, (path, path_profiles) in enumerate(sorted(by_path.items())):
        safe_label = _escape_dot(path)
        lines.append(f"  subgraph cluster_{cluster_idx} {{")
        lines.append(f'    label="{safe_label}";')
        lines.append('    style="rounded,dashed"; color="#64748b"; fontsize=9;')
        for profile in path_profiles:
            nid = _symbol_node_id(profile)
            label = f"{profile.symbol_name}\\n({profile.symbol_kind})"
            lines.append(
                f'    {nid} [shape=box, style="rounded,filled", '
                f'fillcolor="#1e293b", fontcolor="#e2e8f0", label="{label}"];'
            )
        lines.append("  }")
        lines.append("")

    # Collect unique integration types and emit ellipse nodes
    seen_types: set[str] = set()
    for profile in profiles:
        for si in profile.integrations:
            type_val = si.signal.integration_type.value
            if type_val not in seen_types:
                seen_types.add(type_val)
                nid = _integration_node_id(type_val)
                lines.append(
                    f'  {nid} [shape=ellipse, style=filled, '
                    f'fillcolor="#0d9488", fontcolor="white", label="{type_val}"];'
                )

    lines.append("")

    # Edges: symbol -- integration type, styled by confidence
    for profile in profiles:
        sym_nid = _symbol_node_id(profile)
        edge_set: dict[str, str] = {}
        for si in profile.integrations:
            type_val = si.signal.integration_type.value
            if type_val not in edge_set:
                edge_set[type_val] = si.signal.confidence.value

        for type_val, confidence in edge_set.items():
            int_nid = _integration_node_id(type_val)
            style = _CONFIDENCE_STYLE.get(confidence, _DEFAULT_EDGE_STYLE)
            lines.append(
                f'  {sym_nid} -- {int_nid} [{style}, label="{confidence}"];'
            )

    lines.append("}")
    return "\n".join(lines)
