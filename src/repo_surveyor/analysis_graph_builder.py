"""Neo4j persistence for repository survey data."""

import logging
import os
from typing import Protocol

from dotenv import load_dotenv

from .constants import TechLabel
from .ctags import CTagsResult
from .graph_builder import (
    _TechNode,
    build_coarse_structure_graph,
    build_tech_stack_graph,
)
from .integration_detector import IntegrationDetectorResult, detect_integrations
from .integration_patterns import Language
from .pipeline_timer import NullPipelineTimer, PipelineTimer
from .report import SurveyReport
from .surveyor import RepoSurveyor, _normalise_languages
from .symbol_resolver import ResolutionResult, resolve_integration_signals

load_dotenv("../.env")

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

if not logger.handlers:
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.DEBUG)
    formatter = logging.Formatter(
        "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    )
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)


class Neo4jDriver(Protocol):
    """Protocol for Neo4j driver interface."""

    def session(self): ...
    def close(self) -> None: ...


class AnalysisGraphBuilder:
    """Build and persist repository analysis graphs to Neo4j."""

    def __init__(self, driver: Neo4jDriver) -> None:
        """Initialize with a Neo4j driver.

        Args:
            driver: A Neo4j driver instance (or any object implementing Neo4jDriver protocol)
        """
        self.driver = driver

    def close(self) -> None:
        """Close the Neo4j connection."""
        self.driver.close()

    def __enter__(self) -> "AnalysisGraphBuilder":
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.close()

    def persist_tech_stacks(self, report: SurveyReport) -> None:
        """Persist tech stack survey report to Neo4j.

        Creates nodes for:
        - Repository
        - Directory nodes with hierarchy (parent-child relationships)
        - Language, PackageManager, Framework, Infrastructure nodes connected to directories

        Args:
            report: The SurveyReport from tech_stacks()
        """
        directories, dir_relationships, tech_nodes, top_level_dirs = (
            build_tech_stack_graph(report)
        )

        with self.driver.session() as session:
            session.run(
                """
                CREATE (r:Repository {path: $path, name: $name})
                """,
                path=report.repo_path,
                name=report.repo_path.split("/")[-1],
            )

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

            for label in [
                TechLabel.LANGUAGE,
                TechLabel.PACKAGE_MANAGER,
                TechLabel.FRAMEWORK,
                TechLabel.INFRASTRUCTURE,
            ]:
                nodes_of_type = [n for n in tech_nodes if n[_TechNode.TYPE] == label]
                if nodes_of_type:
                    rel_type = nodes_of_type[0][_TechNode.REL_TYPE]
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

        logger.info("Building coarse structure graph in memory...")
        symbols, relationships, top_level_symbols = build_coarse_structure_graph(result)

        if not symbols:
            logger.info("No symbols to persist")
            return

        logger.info(
            f"Graph built: {len(symbols)} symbols, {len(relationships)} relationships, "
            f"{len(top_level_symbols)} top-level symbols"
        )

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

        if top_level_symbols:
            logger.info(
                f"Linking {len(top_level_symbols)} top-level symbols to Repository..."
            )
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
            logger.info(
                f"Linked {len(top_level_symbols)} top-level symbols to Repository"
            )

        logger.info("Coarse structure persistence complete")


def create_analysis_graph_builder(
    uri: str, username: str, password: str
) -> AnalysisGraphBuilder:
    """Factory function to create AnalysisGraphBuilder with a real Neo4j driver.

    Args:
        uri: Neo4j connection URI (e.g., "bolt://localhost:7687")
        username: Neo4j username
        password: Neo4j password

    Returns:
        AnalysisGraphBuilder instance configured with a Neo4j driver
    """
    from neo4j import GraphDatabase

    driver = GraphDatabase.driver(uri, auth=(username, password))
    return AnalysisGraphBuilder(driver)


def survey_and_persist(
    repo_path: str,
    neo4j_uri: str,
    neo4j_username: str,
    neo4j_password: str,
    languages: list[str | Language] = [],
    timer: PipelineTimer = NullPipelineTimer(),
) -> tuple[SurveyReport, CTagsResult, IntegrationDetectorResult, ResolutionResult]:
    """Run tech_stacks(), coarse_structure(), detect_integrations(), and symbol resolution, persisting results to Neo4j.

    Args:
        repo_path: Path to the repository to analyze
        neo4j_uri: Neo4j connection URI
        neo4j_username: Neo4j username
        neo4j_password: Neo4j password
        languages: Languages to filter both coarse_structure() and
                   detect_integrations(). Accepts CTags language name strings
                   (e.g., "Java") and/or Language enum members. Defaults to all.
        timer: Pipeline timing observer for recording stage durations.

    Returns:
        Tuple of (SurveyReport, CTagsResult, IntegrationDetectorResult, ResolutionResult)
    """
    ctags_languages, integration_languages = _normalise_languages(languages)
    surveyor = RepoSurveyor(repo_path)

    timer.stage_started("tech_stacks")
    tech_report = surveyor.tech_stacks()
    timer.stage_completed("tech_stacks")
    logger.info("Tech stacks completed")

    timer.stage_started("coarse_structure")
    structure_result = surveyor.coarse_structure(
        languages=ctags_languages,
        exclude_patterns=[
            "target",
            "resources",
            "node_modules",
            ".mypy_cache",
            ".idea",
            ".dist",
            "gen",
        ],
    )
    timer.stage_completed("coarse_structure")
    logger.info("Coarse structure completed")

    directory_frameworks = {
        m.directory: m.frameworks for m in tech_report.directory_markers if m.frameworks
    }
    timer.stage_started("integration_detection")
    integration_result = detect_integrations(
        repo_path,
        languages=integration_languages,
        directory_frameworks=directory_frameworks,
        timer=timer,
    )
    timer.stage_completed("integration_detection")
    logger.info(
        "Integration detection completed: %d points in %d files",
        len(integration_result.integration_points),
        integration_result.files_scanned,
    )

    timer.stage_started("symbol_resolution")
    resolution = resolve_integration_signals(
        structure_result, integration_result, repo_path
    )
    timer.stage_completed("symbol_resolution")
    logger.info(
        "Symbol resolution completed: %d resolved, %d unresolved, %d profiles",
        len(resolution.resolved),
        len(resolution.unresolved),
        len(resolution.profiles),
    )

    with create_analysis_graph_builder(
        neo4j_uri, neo4j_username, neo4j_password
    ) as builder:
        timer.stage_started("persist_tech_stacks")
        builder.persist_tech_stacks(tech_report)
        timer.stage_completed("persist_tech_stacks")

        timer.stage_started("persist_coarse_structure")
        builder.persist_coarse_structure(structure_result, repo_path)
        timer.stage_completed("persist_coarse_structure")

    return tech_report, structure_result, integration_result, resolution


def main_fn():
    repo = "/Users/asgupta/code/smojol/smojol-core"
    uri = os.environ.get("NEO4J_URI")
    username = os.environ.get("NEO4J_USERNAME")
    password = os.environ.get("NEO4J_PASSWORD")
    print(f"Analyzing repository: {repo}")
    print(f"Connecting to Neo4j at: {uri}")
    tech_report, structure_result, integration_result, resolution = survey_and_persist(
        repo, uri, username, password
    )
    print(f"\nTech Stack Report:")
    print(tech_report.to_text())
    print(f"\nCode Structure:")
    print(f"  Symbols found: {len(structure_result.entries)}")
    if structure_result.entries:
        kinds = {}
        for entry in structure_result.entries:
            kinds[entry.kind] = kinds.get(entry.kind, 0) + 1
        print(f"  Symbol kinds: {kinds}")
    print(f"\nIntegration Detection:")
    print(f"  Files scanned: {integration_result.files_scanned}")
    print(f"  Integration points: {len(integration_result.integration_points)}")


if __name__ == "__main__":
    main_fn()
