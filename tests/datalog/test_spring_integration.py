"""Tests for Datalog-based Spring framework integration detection.

Four test layers:
  1. Bridge fact writing — verifies TSV file generation (no Soufflé).
  2. Annotation detection — @RestController, @GetMapping, etc. → HTTP_REST.
  3. Messaging detection — @KafkaListener → MESSAGING INWARD.
  4. Type reference / instantiation detection — KafkaTemplate, Neo4jRepository.
  5. Composite rules — controller_endpoint links class + method annotations.

All Soufflé-dependent tests are skipped automatically when the ``souffle``
binary is not on PATH.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
from tree_sitter_language_pack import get_parser

from query.datalog_integration_runner import (
    SPRING_BRIDGE_FACTS,
    FrameworkBridgeFacts,
    _write_bridge_facts,
    detect_integrations_datalog,
)
from query.datalog_plugins import make_default_registry
from repo_surveyor.integration_patterns.types import (
    Confidence,
    IntegrationType,
    Language,
    SignalDirection,
)

_JAVA_PLUGIN = make_default_registry().get(Language.JAVA)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

requires_souffle = pytest.mark.skipif(
    shutil.which("souffle") is None,
    reason="souffle binary not in PATH",
)

# A Spring REST controller with @GetMapping and @PostMapping.
_CONTROLLER_JAVA = b"""\
@RestController
public class UserController {

    @GetMapping("/users/{id}")
    public User getUser(@PathVariable String id) {
        return service.findById(id);
    }

    @PostMapping("/users")
    public User createUser(@RequestBody User user) {
        return service.save(user);
    }
}
"""

# A Kafka consumer with @KafkaListener.
_MESSAGING_JAVA = b"""\
@Service
public class OrderConsumer {

    @KafkaListener(topics = "orders")
    public void onOrder(String message) {
        process(message);
    }
}
"""

# A class using KafkaTemplate (field) and RestTemplate (instantiation).
_TEMPLATE_JAVA = b"""\
@Service
public class NotificationService {
    private KafkaTemplate<String, String> kafka;

    public void notify(String msg) {
        kafka.send("topic", msg);
        RestTemplate rest = new RestTemplate();
        rest.getForObject("http://example.com", String.class);
    }
}
"""

# A repository interface extending Neo4jRepository, and a JDBC class.
_DATABASE_JAVA = b"""\
public interface PersonRepo extends Neo4jRepository<Person, Long> {
}

public class JdbcDao {
    private JdbcTemplate jdbc;

    public void insert(String sql) {
        jdbc.update(sql);
    }
}
"""

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def controller_signals(tmp_path: Path) -> list:
    parser = get_parser("java")
    tree = parser.parse(_CONTROLLER_JAVA)
    return detect_integrations_datalog(
        tree.root_node,
        _CONTROLLER_JAVA,
        "UserController.java",
        _JAVA_PLUGIN,
        SPRING_BRIDGE_FACTS,
        tmp_path,
    )


@pytest.fixture()
def messaging_signals(tmp_path: Path) -> list:
    parser = get_parser("java")
    tree = parser.parse(_MESSAGING_JAVA)
    return detect_integrations_datalog(
        tree.root_node,
        _MESSAGING_JAVA,
        "OrderConsumer.java",
        _JAVA_PLUGIN,
        SPRING_BRIDGE_FACTS,
        tmp_path,
    )


@pytest.fixture()
def template_signals(tmp_path: Path) -> list:
    parser = get_parser("java")
    tree = parser.parse(_TEMPLATE_JAVA)
    return detect_integrations_datalog(
        tree.root_node,
        _TEMPLATE_JAVA,
        "NotificationService.java",
        _JAVA_PLUGIN,
        SPRING_BRIDGE_FACTS,
        tmp_path,
    )


@pytest.fixture()
def database_signals(tmp_path: Path) -> list:
    parser = get_parser("java")
    tree = parser.parse(_DATABASE_JAVA)
    return detect_integrations_datalog(
        tree.root_node,
        _DATABASE_JAVA,
        "PersonRepo.java",
        _JAVA_PLUGIN,
        SPRING_BRIDGE_FACTS,
        tmp_path,
    )


# ---------------------------------------------------------------------------
# 1. Bridge fact writing (no Soufflé needed)
# ---------------------------------------------------------------------------


class TestBridgeFactWriting:
    """Verify TSV bridge fact file generation."""

    def test_annotation_facts_file_created(self, tmp_path: Path):
        _write_bridge_facts(SPRING_BRIDGE_FACTS, tmp_path)
        ann_file = tmp_path / "annotation_integration.facts"
        assert ann_file.exists()
        lines = ann_file.read_text(encoding="utf-8").splitlines()
        assert len(lines) == len(SPRING_BRIDGE_FACTS.annotations)

    def test_type_facts_file_created(self, tmp_path: Path):
        _write_bridge_facts(SPRING_BRIDGE_FACTS, tmp_path)
        type_file = tmp_path / "type_integration.facts"
        assert type_file.exists()
        lines = type_file.read_text(encoding="utf-8").splitlines()
        assert len(lines) == len(SPRING_BRIDGE_FACTS.types)

    def test_annotation_facts_tsv_format(self, tmp_path: Path):
        _write_bridge_facts(SPRING_BRIDGE_FACTS, tmp_path)
        ann_file = tmp_path / "annotation_integration.facts"
        for line in ann_file.read_text(encoding="utf-8").splitlines():
            cols = line.split("\t")
            assert (
                len(cols) == 3
            ), f"Expected 3 tab-separated columns, got {len(cols)}: {line}"

    def test_type_facts_tsv_format(self, tmp_path: Path):
        _write_bridge_facts(SPRING_BRIDGE_FACTS, tmp_path)
        type_file = tmp_path / "type_integration.facts"
        for line in type_file.read_text(encoding="utf-8").splitlines():
            cols = line.split("\t")
            assert (
                len(cols) == 3
            ), f"Expected 3 tab-separated columns, got {len(cols)}: {line}"

    def test_specific_annotation_present(self, tmp_path: Path):
        _write_bridge_facts(SPRING_BRIDGE_FACTS, tmp_path)
        content = (tmp_path / "annotation_integration.facts").read_text(
            encoding="utf-8"
        )
        assert "GetMapping\thttp_rest\tinward" in content

    def test_specific_type_present(self, tmp_path: Path):
        _write_bridge_facts(SPRING_BRIDGE_FACTS, tmp_path)
        content = (tmp_path / "type_integration.facts").read_text(encoding="utf-8")
        assert "KafkaTemplate\tmessaging\toutward" in content


# ---------------------------------------------------------------------------
# 2. Annotation detection (@RestController, @GetMapping, etc.)
# ---------------------------------------------------------------------------


class TestAnnotationDetection:
    """Verify annotation-based integration signal detection."""

    @requires_souffle
    def test_rest_controller_detected(self, controller_signals):
        patterns = {s.matched_pattern for s in controller_signals}
        assert "RestController" in patterns

    @requires_souffle
    def test_get_mapping_detected(self, controller_signals):
        patterns = {s.matched_pattern for s in controller_signals}
        assert "GetMapping" in patterns

    @requires_souffle
    def test_post_mapping_detected(self, controller_signals):
        patterns = {s.matched_pattern for s in controller_signals}
        assert "PostMapping" in patterns

    @requires_souffle
    def test_request_body_detected(self, controller_signals):
        patterns = {s.matched_pattern for s in controller_signals}
        assert "RequestBody" in patterns

    @requires_souffle
    def test_path_variable_detected(self, controller_signals):
        patterns = {s.matched_pattern for s in controller_signals}
        assert "PathVariable" in patterns

    @requires_souffle
    def test_all_http_rest_type(self, controller_signals):
        assert all(
            s.integration_type == IntegrationType.HTTP_REST for s in controller_signals
        )

    @requires_souffle
    def test_all_inward_direction(self, controller_signals):
        assert all(s.direction == SignalDirection.INWARD for s in controller_signals)

    @requires_souffle
    def test_all_high_confidence(self, controller_signals):
        assert all(s.confidence == Confidence.HIGH for s in controller_signals)

    @requires_souffle
    def test_source_is_spring_datalog(self, controller_signals):
        assert all(s.source == "Spring/Datalog" for s in controller_signals)

    @requires_souffle
    def test_get_mapping_line_number(self, controller_signals):
        get_mappings = [
            s for s in controller_signals if s.matched_pattern == "GetMapping"
        ]
        assert len(get_mappings) == 1
        # @GetMapping is on line 4 (1-indexed) of _CONTROLLER_JAVA
        assert get_mappings[0].match.line_number == 4

    @requires_souffle
    def test_post_mapping_line_number(self, controller_signals):
        post_mappings = [
            s for s in controller_signals if s.matched_pattern == "PostMapping"
        ]
        assert len(post_mappings) == 1
        # @PostMapping is on line 9 (1-indexed)
        assert post_mappings[0].match.line_number == 9


# ---------------------------------------------------------------------------
# 3. Messaging detection (@KafkaListener)
# ---------------------------------------------------------------------------


class TestMessagingDetection:
    """Verify messaging annotation detection."""

    @requires_souffle
    def test_kafka_listener_detected(self, messaging_signals):
        patterns = {s.matched_pattern for s in messaging_signals}
        assert "KafkaListener" in patterns

    @requires_souffle
    def test_kafka_listener_is_messaging(self, messaging_signals):
        kafka_signals = [
            s for s in messaging_signals if s.matched_pattern == "KafkaListener"
        ]
        assert all(
            s.integration_type == IntegrationType.MESSAGING for s in kafka_signals
        )

    @requires_souffle
    def test_kafka_listener_is_inward(self, messaging_signals):
        kafka_signals = [
            s for s in messaging_signals if s.matched_pattern == "KafkaListener"
        ]
        assert all(s.direction == SignalDirection.INWARD for s in kafka_signals)

    @requires_souffle
    def test_kafka_listener_line_number(self, messaging_signals):
        kafka_signals = [
            s for s in messaging_signals if s.matched_pattern == "KafkaListener"
        ]
        assert len(kafka_signals) == 1
        # @KafkaListener is on line 4 (1-indexed)
        assert kafka_signals[0].match.line_number == 4


# ---------------------------------------------------------------------------
# 4. Type reference detection (KafkaTemplate, Neo4jRepository, etc.)
# ---------------------------------------------------------------------------


class TestTypeReferenceDetection:
    """Verify type-based integration signal detection."""

    @requires_souffle
    def test_kafka_template_detected(self, template_signals):
        patterns = {s.matched_pattern for s in template_signals}
        assert "KafkaTemplate" in patterns

    @requires_souffle
    def test_kafka_template_is_messaging_outward(self, template_signals):
        kafka_signals = [
            s for s in template_signals if s.matched_pattern == "KafkaTemplate"
        ]
        assert all(
            s.integration_type == IntegrationType.MESSAGING for s in kafka_signals
        )
        assert all(s.direction == SignalDirection.OUTWARD for s in kafka_signals)

    @requires_souffle
    def test_rest_template_detected(self, template_signals):
        patterns = {s.matched_pattern for s in template_signals}
        assert "RestTemplate" in patterns

    @requires_souffle
    def test_rest_template_is_http_outward(self, template_signals):
        rest_signals = [
            s for s in template_signals if s.matched_pattern == "RestTemplate"
        ]
        assert all(
            s.integration_type == IntegrationType.HTTP_REST for s in rest_signals
        )
        assert all(s.direction == SignalDirection.OUTWARD for s in rest_signals)

    @requires_souffle
    def test_neo4j_repository_detected(self, database_signals):
        patterns = {s.matched_pattern for s in database_signals}
        assert "Neo4jRepository" in patterns

    @requires_souffle
    def test_neo4j_repository_is_database_outward(self, database_signals):
        neo4j_signals = [
            s for s in database_signals if s.matched_pattern == "Neo4jRepository"
        ]
        assert all(
            s.integration_type == IntegrationType.DATABASE for s in neo4j_signals
        )
        assert all(s.direction == SignalDirection.OUTWARD for s in neo4j_signals)

    @requires_souffle
    def test_jdbc_template_detected(self, database_signals):
        patterns = {s.matched_pattern for s in database_signals}
        assert "JdbcTemplate" in patterns

    @requires_souffle
    def test_jdbc_template_is_database_outward(self, database_signals):
        jdbc_signals = [
            s for s in database_signals if s.matched_pattern == "JdbcTemplate"
        ]
        assert all(s.integration_type == IntegrationType.DATABASE for s in jdbc_signals)
        assert all(s.direction == SignalDirection.OUTWARD for s in jdbc_signals)


# ---------------------------------------------------------------------------
# 5. Composite rules (controller_endpoint)
# ---------------------------------------------------------------------------


class TestCompositeRules:
    """Verify controller_endpoint links class and method annotations."""

    @requires_souffle
    def test_controller_endpoint_output_exists(self, tmp_path: Path):
        from query.treesitter_to_datalog import emit_datalog, run_souffle

        parser = get_parser("java")
        tree = parser.parse(_CONTROLLER_JAVA)
        facts_dir = tmp_path / "facts"
        facts_dir.mkdir()
        facts = emit_datalog(tree.root_node, _CONTROLLER_JAVA, plugin=_JAVA_PLUGIN)
        facts.to_souffle_facts(facts_dir, plugin=_JAVA_PLUGIN)
        _write_bridge_facts(SPRING_BRIDGE_FACTS, facts_dir)
        dl_file = (
            Path(__file__).resolve().parent.parent.parent
            / "query"
            / "spring_integration.dl"
        )
        results = run_souffle(dl_file, facts_dir, tmp_path / "out")
        endpoints = results.get("controller_endpoint", [])
        assert (
            len(endpoints) >= 2
        ), f"Expected at least 2 controller endpoints, got {len(endpoints)}"

    @requires_souffle
    def test_controller_endpoint_has_class_name(self, tmp_path: Path):
        from query.treesitter_to_datalog import emit_datalog, run_souffle

        parser = get_parser("java")
        tree = parser.parse(_CONTROLLER_JAVA)
        facts_dir = tmp_path / "facts"
        facts_dir.mkdir()
        facts = emit_datalog(tree.root_node, _CONTROLLER_JAVA, plugin=_JAVA_PLUGIN)
        facts.to_souffle_facts(facts_dir, plugin=_JAVA_PLUGIN)
        _write_bridge_facts(SPRING_BRIDGE_FACTS, facts_dir)
        dl_file = (
            Path(__file__).resolve().parent.parent.parent
            / "query"
            / "spring_integration.dl"
        )
        results = run_souffle(dl_file, facts_dir, tmp_path / "out")
        endpoints = results.get("controller_endpoint", [])
        # Column 2 (0-indexed) is class_name
        class_names = {row[2] for row in endpoints}
        assert "UserController" in class_names

    @requires_souffle
    def test_controller_endpoint_has_method_names(self, tmp_path: Path):
        from query.treesitter_to_datalog import emit_datalog, run_souffle

        parser = get_parser("java")
        tree = parser.parse(_CONTROLLER_JAVA)
        facts_dir = tmp_path / "facts"
        facts_dir.mkdir()
        facts = emit_datalog(tree.root_node, _CONTROLLER_JAVA, plugin=_JAVA_PLUGIN)
        facts.to_souffle_facts(facts_dir, plugin=_JAVA_PLUGIN)
        _write_bridge_facts(SPRING_BRIDGE_FACTS, facts_dir)
        dl_file = (
            Path(__file__).resolve().parent.parent.parent
            / "query"
            / "spring_integration.dl"
        )
        results = run_souffle(dl_file, facts_dir, tmp_path / "out")
        endpoints = results.get("controller_endpoint", [])
        # Column 3 (0-indexed) is method_name
        method_names = {row[3] for row in endpoints}
        assert "getUser" in method_names
        assert "createUser" in method_names

    @requires_souffle
    def test_controller_endpoint_has_http_verbs(self, tmp_path: Path):
        from query.treesitter_to_datalog import emit_datalog, run_souffle

        parser = get_parser("java")
        tree = parser.parse(_CONTROLLER_JAVA)
        facts_dir = tmp_path / "facts"
        facts_dir.mkdir()
        facts = emit_datalog(tree.root_node, _CONTROLLER_JAVA, plugin=_JAVA_PLUGIN)
        facts.to_souffle_facts(facts_dir, plugin=_JAVA_PLUGIN)
        _write_bridge_facts(SPRING_BRIDGE_FACTS, facts_dir)
        dl_file = (
            Path(__file__).resolve().parent.parent.parent
            / "query"
            / "spring_integration.dl"
        )
        results = run_souffle(dl_file, facts_dir, tmp_path / "out")
        endpoints = results.get("controller_endpoint", [])
        # Column 4 (0-indexed) is http_verb
        verbs = {row[4] for row in endpoints}
        assert "GetMapping" in verbs
        assert "PostMapping" in verbs
