"""Neo4j persistence for repository survey data."""

import logging
import os

from dotenv import load_dotenv
from neo4j import GraphDatabase

from .ctags import CTagsResult
from .report import SurveyReport
from .surveyor import RepoSurveyor

load_dotenv("../.env")

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

# Add console handler if not already present
if not logger.handlers:
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.DEBUG)
    formatter = logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)


class Neo4jPersistence:
    """Persist repository survey data to Neo4j."""

    def __init__(self, uri: str, username: str, password: str) -> None:
        """Initialize Neo4j connection.

        Args:
            uri: Neo4j connection URI (e.g., "bolt://localhost:7687")
            username: Neo4j username
            password: Neo4j password
        """
        self.driver = GraphDatabase.driver(uri, auth=(username, password))

    def close(self) -> None:
        """Close the Neo4j connection."""
        self.driver.close()

    def __enter__(self) -> "Neo4jPersistence":
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.close()

    def _build_tech_stack_graph(
            self, report: SurveyReport
    ) -> tuple[list[dict], list[dict], list[dict], set[str]]:
        """Build tech stack graph data in memory.

        Returns:
            Tuple of:
            - directories: List of unique directory node dicts
            - dir_relationships: List of parent-child directory relationships
            - tech_nodes: List of technology nodes with their directory relationships
            - top_level_dirs: Set of top-level directory paths (link directly to Repository)
        """
        # Track unique directories by path
        directories: dict[str, dict] = {}
        dir_relationships: list[dict] = []
        tech_nodes: list[dict] = []
        top_level_dirs: set[str] = set()

        # Process each marker and build hierarchy
        for marker in report.directory_markers:
            path = marker.directory

            # Skip root directory "."
            if path == ".":
                continue

            # Add the marker's directory with its properties
            if path not in directories:
                directories[path] = {
                    "path": path,
                    "name": path.split("/")[-1],
                    "marker_file": marker.marker_file,
                }
            else:
                # Directory exists, update marker_file if not set
                if directories[path]["marker_file"] is None:
                    directories[path]["marker_file"] = marker.marker_file

            # Walk up the path to create intermediate directories
            current = path
            while "/" in current:
                parent = current.rsplit("/", 1)[0]
                if parent:
                    # Add parent directory if not exists
                    if parent not in directories:
                        directories[parent] = {
                            "path": parent,
                            "name": parent.split("/")[-1],
                            "marker_file": None,
                        }
                    # Add relationship (avoid duplicates)
                    rel = {"parent": parent, "child": current}
                    if rel not in dir_relationships:
                        dir_relationships.append(rel)
                    current = parent
                else:
                    break

            # Track top-level directory (links directly to Repository)
            top_level = path.split("/")[0] if "/" in path else path
            top_level_dirs.add(top_level)

            # Collect technology nodes for this directory
            for language in marker.languages:
                tech_nodes.append({
                    "directory": path,
                    "type": "Language",
                    "name": language,
                    "rel_type": "USES_LANGUAGE",
                })
            for pm in marker.package_managers:
                tech_nodes.append({
                    "directory": path,
                    "type": "PackageManager",
                    "name": pm,
                    "rel_type": "USES_PACKAGE_MANAGER",
                })
            for framework in marker.frameworks:
                tech_nodes.append({
                    "directory": path,
                    "type": "Framework",
                    "name": framework,
                    "rel_type": "USES_FRAMEWORK",
                })
            for infra in marker.infrastructure:
                tech_nodes.append({
                    "directory": path,
                    "type": "Infrastructure",
                    "name": infra,
                    "rel_type": "USES_INFRASTRUCTURE",
                })

        return list(directories.values()), dir_relationships, tech_nodes, top_level_dirs

    def persist_tech_stacks(self, report: SurveyReport) -> None:
        """Persist tech stack survey report to Neo4j.

        Creates nodes for:
        - Repository
        - Directory nodes with hierarchy (parent-child relationships)
        - Language, PackageManager, Framework, Infrastructure nodes connected to directories

        Args:
            report: The SurveyReport from tech_stacks()
        """
        # Build entire graph in memory first
        directories, dir_relationships, tech_nodes, top_level_dirs = self._build_tech_stack_graph(report)

        with self.driver.session() as session:
            # Create Repository node
            session.run(
                """
                CREATE (r:Repository {path: $path, name: $name})
                """,
                path=report.repo_path,
                name=report.repo_path.split("/")[-1],
            )

            # Batch create all Directory nodes
            if directories:
                session.run(
                    """
                    UNWIND $directories AS dir
                    CREATE (d:Directory {
                        path: dir.path,
                        name: dir.name,
                        marker_file: dir.marker_file,
                        repo_path: $repo_path
                    })
                    """,
                    repo_path=report.repo_path,
                    directories=directories,
                )

            # Batch create directory hierarchy relationships
            if dir_relationships:
                session.run(
                    """
                    UNWIND $relationships AS rel
                    MATCH (parent:Directory {path: rel.parent, repo_path: $repo_path})
                    MATCH (child:Directory {path: rel.child, repo_path: $repo_path})
                    CREATE (parent)-[:CONTAINS_DIRECTORY]->(child)
                    """,
                    repo_path=report.repo_path,
                    relationships=dir_relationships,
                )

            # Link top-level directories directly to repository
            if top_level_dirs:
                session.run(
                    """
                    UNWIND $top_level AS dir_path
                    MATCH (r:Repository {path: $repo_path})
                    MATCH (d:Directory {path: dir_path, repo_path: $repo_path})
                    CREATE (r)-[:CONTAINS_DIRECTORY]->(d)
                    """,
                    repo_path=report.repo_path,
                    top_level=list(top_level_dirs),
                )

            # Batch create technology nodes and relationships
            for label in ["Language", "PackageManager", "Framework", "Infrastructure"]:
                nodes_of_type = [n for n in tech_nodes if n["type"] == label]
                if nodes_of_type:
                    rel_type = nodes_of_type[0]["rel_type"]
                    session.run(
                        f"""
                        UNWIND $nodes AS node
                        MATCH (d:Directory {{path: node.directory, repo_path: $repo_path}})
                        CREATE (t:{label} {{name: node.name}})
                        CREATE (d)-[:{rel_type}]->(t)
                        """,
                        repo_path=report.repo_path,
                        nodes=nodes_of_type,
                    )

    def _extract_package(self, file_path: str, language: str | None) -> str | None:
        """Extract package name from file path based on language conventions.

        Args:
            file_path: The file path relative to repository root
            language: The programming language of the file

        Returns:
            Package name in dot notation, or None if not determinable
        """
        if not language:
            return None

        # Get directory path (without filename)
        if "/" in file_path:
            dir_path = file_path.rsplit("/", 1)[0]
        else:
            return None

        if language.lower() == "java":
            # Java: look for src/main/java/, src/test/java/, or just src/
            java_markers = ["src/main/java/", "src/test/java/", "src/"]
            for marker in java_markers:
                if marker in dir_path:
                    package_path = dir_path.split(marker, 1)[-1]
                    return package_path.replace("/", ".") if package_path else None
            # Fallback: use full directory path as package
            return dir_path.replace("/", ".")

        elif language.lower() == "python":
            # Python: use directory path, excluding common source dirs
            python_markers = ["src/", "lib/"]
            for marker in python_markers:
                if dir_path.startswith(marker):
                    package_path = dir_path[len(marker):]
                    return package_path.replace("/", ".") if package_path else None
            return dir_path.replace("/", ".")

        elif language.lower() in ("c#", "csharp"):
            # C#: namespace usually matches directory structure
            return dir_path.replace("/", ".")

        # Default: use directory path as package
        return dir_path.replace("/", ".")

    def _build_coarse_structure_graph(
            self, result: CTagsResult
    ) -> tuple[list[dict], list[dict], set[str]]:
        """Build coarse structure graph data in memory.

        Args:
            result: The CTagsResult from coarse_structure()

        Returns:
            Tuple of:
            - symbols: List of unique symbol node dicts
            - relationships: List of parent-child relationships based on scope
            - top_level_symbols: Set of symbol IDs that have no parent (link to Repository)
        """
        # Build unique symbols keyed by (path, name, kind, line) to handle overloads
        logger.debug("Indexing symbols...")
        symbols: dict[str, dict] = {}
        symbol_index: dict[tuple[str, str], list[dict]] = {}  # (path, name) -> symbols

        for entry in result.entries:
            # Create unique ID for each symbol
            symbol_id = f"{entry.path}:{entry.name}:{entry.kind}:{entry.line}"

            # Extract package from file path
            package = self._extract_package(entry.path, entry.language)

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

            # Index by (path, name) for parent lookup
            key = (entry.path, entry.name)
            if key not in symbol_index:
                symbol_index[key] = []
            symbol_index[key].append(symbol)

        # Build parent-child relationships based on scope
        logger.debug(f"Indexed {len(symbols)} symbols, resolving parent-child relationships...")
        relationships: list[dict] = []
        symbols_with_parents: set[str] = set()
        for symbol in symbols.values():
            if symbol["scope"]:
                # Find potential parents in the same file with matching name
                parent_key = (symbol["path"], symbol["scope"])
                potential_parents = symbol_index.get(parent_key, [])

                # Filter by scope_kind if specified
                if symbol["scope_kind"]:
                    matching_parents = [p for p in potential_parents if p["kind"] == symbol["scope_kind"]]
                else:
                    matching_parents = potential_parents

                # Create relationship for each matching parent
                for parent in matching_parents:
                    relationships.append({
                        "child_id": symbol["id"],
                        "parent_id": parent["id"],
                    })
                    symbols_with_parents.add(symbol["id"])

        # Identify top-level symbols (no parent)
        top_level_symbols = set(symbols.keys()) - symbols_with_parents
        logger.debug(f"Resolved {len(relationships)} relationships, {len(top_level_symbols)} top-level symbols")

        return list(symbols.values()), relationships, top_level_symbols

    def persist_coarse_structure(self, result: CTagsResult, repo_path: str) -> None:
        """Persist coarse structure (CTags) results to Neo4j.

        Creates CodeSymbol nodes for each entry. Uses the 'scope' field
        to establish parent-child relationships between symbols.
        Each operation runs in its own transaction.

        Args:
            result: The CTagsResult from coarse_structure()
            repo_path: The repository path for linking symbols
        """
        if not result.success:
            raise ValueError(f"CTags failed: {result.error_output}")

        # Build entire graph in memory first
        logger.info("Building coarse structure graph in memory...")
        symbols, relationships, top_level_symbols = self._build_coarse_structure_graph(result)

        if not symbols:
            logger.info("No symbols to persist")
            return

        logger.info(
            f"Graph built: {len(symbols)} symbols, {len(relationships)} relationships, "
            f"{len(top_level_symbols)} top-level symbols"
        )

        # Transaction 1: Create all CodeSymbol nodes
        logger.info(f"Creating {len(symbols)} CodeSymbol nodes...")
        with self.driver.session() as session:
            session.run(
                """
                UNWIND $symbols AS sym
                CREATE (s:CodeSymbol {
                    id: sym.id,
                    name: sym.name,
                    path: sym.path,
                    kind: sym.kind,
                    line: sym.line,
                    signature: sym.signature,
                    language: sym.language,
                    scope: sym.scope,
                    scope_kind: sym.scope_kind,
                    package: sym.package,
                    repo_path: $repo_path
                })
                """,
                repo_path=repo_path,
                symbols=symbols,
            )
        logger.info(f"Created {len(symbols)} CodeSymbol nodes")

        # Transaction 2: Create parent-child relationships based on scope
        if relationships:
            logger.info(f"Creating {len(relationships)} parent-child relationships...")
            with self.driver.session() as session:
                session.run(
                    """
                    UNWIND $relationships AS rel
                    MATCH (child:CodeSymbol {id: rel.child_id, repo_path: $repo_path})
                    MATCH (parent:CodeSymbol {id: rel.parent_id, repo_path: $repo_path})
                    CREATE (parent)-[:CONTAINS]->(child)
                    """,
                    repo_path=repo_path,
                    relationships=relationships,
                )
            logger.info(f"Created {len(relationships)} parent-child relationships")

        # Transaction 3: Link top-level symbols directly to Repository
        if top_level_symbols:
            logger.info(f"Linking {len(top_level_symbols)} top-level symbols to Repository...")
            with self.driver.session() as session:
                session.run(
                    """
                    UNWIND $top_level AS sym_id
                    MATCH (r:Repository {path: $repo_path})
                    MATCH (s:CodeSymbol {id: sym_id, repo_path: $repo_path})
                    CREATE (r)-[:CONTAINS]->(s)
                    """,
                    repo_path=repo_path,
                    top_level=list(top_level_symbols),
                )
            logger.info(f"Linked {len(top_level_symbols)} top-level symbols to Repository")

        logger.info("Coarse structure persistence complete")


def survey_and_persist(
        repo_path: str,
        neo4j_uri: str,
        neo4j_username: str,
        neo4j_password: str,
        languages: list[str],
) -> tuple[SurveyReport, CTagsResult]:
    """Run tech_stacks() and coarse_structure(), persisting results to Neo4j.

    Args:
        repo_path: Path to the repository to analyze
        neo4j_uri: Neo4j connection URI
        neo4j_username: Neo4j username
        neo4j_password: Neo4j password
        languages: Optional list of languages for coarse_structure()

    Returns:
        Tuple of (SurveyReport, CTagsResult)
    """
    surveyor = RepoSurveyor(repo_path)

    # Run tech_stacks()
    tech_report = surveyor.tech_stacks()
    logger.info("Tech stacks completed")
    # Run coarse_structure()
    structure_result = surveyor.coarse_structure(languages=["Java"],
                                                 exclude_patterns=["target", "resources", "node_modules", ".mypy_cache",
                                                                   ".idea", ".dist", "gen"])
    logger.info("Coarse structure completed")
    # Persist to Neo4j
    with Neo4jPersistence(neo4j_uri, neo4j_username, neo4j_password) as persistence:
        persistence.persist_tech_stacks(tech_report)
        persistence.persist_coarse_structure(structure_result, repo_path)

    return tech_report, structure_result


def main_fn():
    # Get configuration from environment or command line
    repo = "/Users/asgupta/code/smojol/smojol-core"
    uri = os.environ.get("NEO4J_URI")
    username = os.environ.get("NEO4J_USERNAME")
    password = os.environ.get("NEO4J_PASSWORD")
    print(f"Analyzing repository: {repo}")
    print(f"Connecting to Neo4j at: {uri}")
    tech_report, structure_result = survey_and_persist(repo, uri, username, password)
    print(f"\nTech Stack Report:")
    print(tech_report.to_text())
    print(f"\nCode Structure:")
    print(f"  Symbols found: {len(structure_result.entries)}")
    if structure_result.entries:
        kinds = {}
        for entry in structure_result.entries:
            kinds[entry.kind] = kinds.get(entry.kind, 0) + 1
        print(f"  Symbol kinds: {kinds}")


if __name__ == "__main__":
    main_fn()
