"""Datalog-based Spring framework integration detection.

Uses the tree-sitter → Soufflé pipeline to detect Spring integration
patterns (HTTP_REST, MESSAGING, DATABASE) via structural AST queries
rather than regex.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from tree_sitter import Node

from query.datalog_plugins import DatalogLanguagePlugin
from query.treesitter_to_datalog import emit_datalog, run_souffle
from repo_surveyor.detection.integration_detector import (
    EntityType,
    FileMatch,
    IntegrationSignal,
)
from repo_surveyor.integration_patterns.types import (
    Confidence,
    IntegrationType,
    Language,
    SignalDirection,
)

_SCRIPTS_DIR = Path(__file__).resolve().parent
_DL_FILE = _SCRIPTS_DIR / "spring_integration.dl"

_INTEGRATION_TYPE_LOOKUP: dict[str, IntegrationType] = {
    t.value: t for t in IntegrationType
}
_DIRECTION_LOOKUP: dict[str, SignalDirection] = {d.value: d for d in SignalDirection}


# ---------------------------------------------------------------------------
# Bridge fact data model
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class AnnotationMapping:
    """Maps an annotation name to an integration type and direction."""

    annotation_name: str
    integration_type: IntegrationType
    direction: SignalDirection


@dataclass(frozen=True)
class TypeMapping:
    """Maps a type name to an integration type and direction."""

    type_name: str
    integration_type: IntegrationType
    direction: SignalDirection


@dataclass(frozen=True)
class FrameworkBridgeFacts:
    """Bridge facts that parameterise the Datalog rules for a framework."""

    annotations: tuple[AnnotationMapping, ...]
    types: tuple[TypeMapping, ...]


# ---------------------------------------------------------------------------
# Spring bridge facts
# ---------------------------------------------------------------------------

SPRING_BRIDGE_FACTS = FrameworkBridgeFacts(
    annotations=(
        AnnotationMapping(
            "RestController", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "Controller", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "GetMapping", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "PostMapping", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "PutMapping", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "DeleteMapping", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "PatchMapping", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "RequestMapping", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "RequestBody", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "PathVariable", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "RequestParam", IntegrationType.HTTP_REST, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "KafkaListener", IntegrationType.MESSAGING, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "RabbitListener", IntegrationType.MESSAGING, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "JmsListener", IntegrationType.MESSAGING, SignalDirection.INWARD
        ),
        AnnotationMapping(
            "Transactional", IntegrationType.DATABASE, SignalDirection.AMBIGUOUS
        ),
    ),
    types=(
        TypeMapping(
            "KafkaTemplate", IntegrationType.MESSAGING, SignalDirection.OUTWARD
        ),
        TypeMapping(
            "RabbitTemplate", IntegrationType.MESSAGING, SignalDirection.OUTWARD
        ),
        TypeMapping("JmsTemplate", IntegrationType.MESSAGING, SignalDirection.OUTWARD),
        TypeMapping("JdbcTemplate", IntegrationType.DATABASE, SignalDirection.OUTWARD),
        TypeMapping(
            "NamedParameterJdbcTemplate",
            IntegrationType.DATABASE,
            SignalDirection.OUTWARD,
        ),
        TypeMapping("JpaRepository", IntegrationType.DATABASE, SignalDirection.OUTWARD),
        TypeMapping(
            "CrudRepository", IntegrationType.DATABASE, SignalDirection.OUTWARD
        ),
        TypeMapping(
            "Neo4jRepository", IntegrationType.DATABASE, SignalDirection.OUTWARD
        ),
        TypeMapping("RestTemplate", IntegrationType.HTTP_REST, SignalDirection.OUTWARD),
        TypeMapping("WebClient", IntegrationType.HTTP_REST, SignalDirection.OUTWARD),
    ),
)


def _write_bridge_facts(bridge: FrameworkBridgeFacts, facts_dir: Path) -> None:
    """Write annotation_integration.facts and type_integration.facts TSVs."""
    annotation_rows = [
        f"{m.annotation_name}\t{m.integration_type.value}\t{m.direction.value}"
        for m in bridge.annotations
    ]
    type_rows = [
        f"{m.type_name}\t{m.integration_type.value}\t{m.direction.value}"
        for m in bridge.types
    ]
    (facts_dir / "annotation_integration.facts").write_text(
        "\n".join(annotation_rows) + ("\n" if annotation_rows else ""),
        encoding="utf-8",
    )
    (facts_dir / "type_integration.facts").write_text(
        "\n".join(type_rows) + ("\n" if type_rows else ""),
        encoding="utf-8",
    )


def _source_line_at(source: bytes, row_0indexed: int) -> str:
    """Extract a single source line (0-indexed row) as a stripped string."""
    lines = source.split(b"\n")
    return (
        lines[row_0indexed].decode("utf-8", errors="replace").strip()
        if row_0indexed < len(lines)
        else ""
    )


def detect_integrations_datalog(
    root_node: Node,
    source: bytes,
    file_path: str,
    plugin: DatalogLanguagePlugin,
    bridge: FrameworkBridgeFacts,
    work_dir: Path,
) -> list[IntegrationSignal]:
    """Detect integration signals using the Datalog pipeline.

    Parameters
    ----------
    root_node:
        Tree-sitter root node for the parsed file.
    source:
        Raw source bytes (needed for line content extraction).
    file_path:
        Path to the source file (used in FileMatch).
    plugin:
        Language plugin (e.g., Java) for the emitter.
    bridge:
        Framework bridge facts mapping annotations/types to integration types.
    work_dir:
        Temporary directory for facts and Soufflé output.
    """
    facts_dir = work_dir / "facts"
    facts_dir.mkdir(parents=True, exist_ok=True)
    out_dir = work_dir / "out"

    facts = emit_datalog(root_node, source, plugin=plugin)
    facts.to_souffle_facts(facts_dir, plugin=plugin)
    _write_bridge_facts(bridge, facts_dir)

    results = run_souffle(_DL_FILE, facts_dir, out_dir)

    return [
        IntegrationSignal(
            match=FileMatch(
                file_path=file_path,
                line_number=int(row[4]) + 1,
                line_content=_source_line_at(source, int(row[4])),
                language=Language.JAVA,
            ),
            integration_type=_INTEGRATION_TYPE_LOOKUP[row[1]],
            confidence=Confidence.HIGH,
            matched_pattern=row[3],
            entity_type=EntityType.FILE_CONTENT,
            source="Spring/Datalog",
            direction=_DIRECTION_LOOKUP[row[2]],
        )
        for row in results.get("integration_signal", [])
    ]
