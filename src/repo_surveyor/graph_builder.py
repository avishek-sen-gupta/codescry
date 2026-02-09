"""Graph building utilities for repository survey data.

Pure functions that transform survey data into graph representations.
These are independent of any specific graph database.
"""

from .ctags import CTagsResult
from .report import SurveyReport


def extract_package(file_path: str, language: str | None) -> str | None:
    """Extract package name from file path based on language conventions.

    Args:
        file_path: The file path relative to repository root
        language: The programming language of the file

    Returns:
        Package name in dot notation, or None if not determinable
    """
    if not language:
        return None

    if "/" not in file_path:
        return None

    dir_path = file_path.rsplit("/", 1)[0]

    extractors = {
        "java": _extract_java_package,
        "python": _extract_python_package,
        "c#": _extract_dotnet_package,
        "csharp": _extract_dotnet_package,
    }

    extractor = extractors.get(language.lower(), _extract_default_package)
    return extractor(dir_path)


def _extract_java_package(dir_path: str) -> str | None:
    """Extract Java package from directory path."""
    java_markers = ["src/main/java/", "src/test/java/", "src/"]
    for marker in java_markers:
        if marker in dir_path:
            package_path = dir_path.split(marker, 1)[-1]
            return package_path.replace("/", ".") if package_path else None
    return dir_path.replace("/", ".")


def _extract_python_package(dir_path: str) -> str | None:
    """Extract Python package from directory path."""
    python_markers = ["src/", "lib/"]
    for marker in python_markers:
        if dir_path.startswith(marker):
            package_path = dir_path[len(marker) :]
            return package_path.replace("/", ".") if package_path else None
    return dir_path.replace("/", ".")


def _extract_dotnet_package(dir_path: str) -> str:
    """Extract .NET namespace from directory path."""
    return dir_path.replace("/", ".")


def _extract_default_package(dir_path: str) -> str:
    """Default package extraction using directory path."""
    return dir_path.replace("/", ".")


def build_tech_stack_graph(
    report: SurveyReport,
) -> tuple[list[dict], list[dict], list[dict], set[str]]:
    """Build tech stack graph data from a survey report.

    Args:
        report: The SurveyReport from tech_stacks()

    Returns:
        Tuple of:
        - directories: List of unique directory node dicts
        - dir_relationships: List of parent-child directory relationships
        - tech_nodes: List of technology nodes with their directory relationships
        - top_level_dirs: Set of top-level directory paths (link directly to Repository)
    """
    directories: dict[str, dict] = {}
    dir_relationships: list[dict] = []
    tech_nodes: list[dict] = []
    top_level_dirs: set[str] = set()

    for marker in report.directory_markers:
        path = marker.directory

        if path == ".":
            continue

        directories = _add_directory(directories, path, marker.marker_file)
        dir_relationships = _build_directory_hierarchy(
            directories, dir_relationships, path
        )
        top_level_dirs.add(_get_top_level_dir(path))
        tech_nodes = _collect_tech_nodes(tech_nodes, path, marker)

    return list(directories.values()), dir_relationships, tech_nodes, top_level_dirs


def _add_directory(
    directories: dict[str, dict], path: str, marker_file: str | None
) -> dict[str, dict]:
    """Add a directory to the directories dict if not present."""
    if path not in directories:
        return {
            **directories,
            path: {
                "path": path,
                "name": path.split("/")[-1],
                "marker_file": marker_file,
            },
        }

    if directories[path]["marker_file"] is None and marker_file is not None:
        updated = {**directories[path], "marker_file": marker_file}
        return {**directories, path: updated}

    return directories


def _build_directory_hierarchy(
    directories: dict[str, dict],
    relationships: list[dict],
    path: str,
) -> list[dict]:
    """Walk up the path to create intermediate directories and relationships."""
    current = path
    new_relationships = list(relationships)
    new_directories = dict(directories)

    while "/" in current:
        parent = current.rsplit("/", 1)[0]
        if not parent:
            break

        if parent not in new_directories:
            new_directories[parent] = {
                "path": parent,
                "name": parent.split("/")[-1],
                "marker_file": None,
            }

        rel = {"parent": parent, "child": current}
        if rel not in new_relationships:
            new_relationships.append(rel)

        current = parent

    directories.update(new_directories)
    return new_relationships


def _get_top_level_dir(path: str) -> str:
    """Get the top-level directory from a path."""
    return path.split("/")[0] if "/" in path else path


def _collect_tech_nodes(tech_nodes: list[dict], path: str, marker) -> list[dict]:
    """Collect technology nodes for a directory marker."""
    new_nodes = list(tech_nodes)

    tech_mappings = [
        (marker.languages, "Language", "USES_LANGUAGE"),
        (marker.package_managers, "PackageManager", "USES_PACKAGE_MANAGER"),
        (marker.frameworks, "Framework", "USES_FRAMEWORK"),
        (marker.infrastructure, "Infrastructure", "USES_INFRASTRUCTURE"),
    ]

    for items, tech_type, rel_type in tech_mappings:
        for item in items:
            new_nodes.append(
                {
                    "directory": path,
                    "type": tech_type,
                    "name": item,
                    "rel_type": rel_type,
                }
            )

    return new_nodes


def build_coarse_structure_graph(
    result: CTagsResult,
) -> tuple[list[dict], list[dict], set[str]]:
    """Build coarse structure graph data from CTags result.

    Args:
        result: The CTagsResult from coarse_structure()

    Returns:
        Tuple of:
        - symbols: List of unique symbol node dicts
        - relationships: List of parent-child relationships based on scope
        - top_level_symbols: Set of symbol IDs that have no parent (link to Repository)
    """
    symbols, symbol_index = _index_symbols(result.entries)
    relationships, symbols_with_parents = _resolve_relationships(symbols, symbol_index)
    top_level_symbols = set(symbols.keys()) - symbols_with_parents

    return list(symbols.values()), relationships, top_level_symbols


def _index_symbols(
    entries: list,
) -> tuple[dict[str, dict], dict[tuple[str, str], list[dict]]]:
    """Index symbols by ID and by (path, name) for parent lookup."""
    symbols: dict[str, dict] = {}
    symbol_index: dict[tuple[str, str], list[dict]] = {}

    for entry in entries:
        symbol_id = f"{entry.path}:{entry.name}:{entry.kind}:{entry.line}"
        package = extract_package(entry.path, entry.language)

        symbol = {
            "id": symbol_id,
            "name": entry.name,
            "path": entry.path,
            "kind": entry.kind,
            "line": entry.line,
            "signature": entry.signature,
            "language": entry.language,
            "scope": entry.scope,
            "scope_kind": entry.scope_kind,
            "package": package,
        }
        symbols[symbol_id] = symbol

        key = (entry.path, entry.name)
        if key not in symbol_index:
            symbol_index[key] = []
        symbol_index[key].append(symbol)

    return symbols, symbol_index


def _resolve_relationships(
    symbols: dict[str, dict],
    symbol_index: dict[tuple[str, str], list[dict]],
) -> tuple[list[dict], set[str]]:
    """Resolve parent-child relationships based on scope."""
    relationships: list[dict] = []
    symbols_with_parents: set[str] = set()

    for symbol in symbols.values():
        if not symbol["scope"]:
            continue

        parent_key = (symbol["path"], symbol["scope"])
        potential_parents = symbol_index.get(parent_key, [])

        matching_parents = (
            [p for p in potential_parents if p["kind"] == symbol["scope_kind"]]
            if symbol["scope_kind"]
            else potential_parents
        )

        for parent in matching_parents:
            relationships.append(
                {
                    "child_id": symbol["id"],
                    "parent_id": parent["id"],
                }
            )
            symbols_with_parents.add(symbol["id"])

    return relationships, symbols_with_parents
