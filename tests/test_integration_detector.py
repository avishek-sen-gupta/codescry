"""Tests for integration point detection from file contents."""

import sys
import tempfile
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from repo_surveyor.integration_detector import (
    Confidence,
    EntityType,
    FileMatch,
    IntegrationDetectorResult,
    IntegrationPoint,
    IntegrationType,
    classify_directory,
    detect_integrations,
    get_language_from_extension,
    get_patterns_for_language,
    scan_file_for_integrations,
)


class TestGetLanguageFromExtension:
    """Tests for language detection from file extension."""

    def test_java_extension(self) -> None:
        """Should detect Java from .java extension."""
        assert get_language_from_extension("Test.java") == "Java"
        assert get_language_from_extension("/path/to/Test.java") == "Java"

    def test_rust_extension(self) -> None:
        """Should detect Rust from .rs extension."""
        assert get_language_from_extension("main.rs") == "Rust"

    def test_python_extension(self) -> None:
        """Should detect Python from .py extension."""
        assert get_language_from_extension("app.py") == "Python"

    def test_typescript_extension(self) -> None:
        """Should detect TypeScript from .ts and .tsx extensions."""
        assert get_language_from_extension("app.ts") == "TypeScript"
        assert get_language_from_extension("component.tsx") == "TypeScript"

    def test_javascript_extension(self) -> None:
        """Should detect JavaScript from .js and .jsx extensions."""
        assert get_language_from_extension("app.js") == "JavaScript"
        assert get_language_from_extension("component.jsx") == "JavaScript"

    def test_go_extension(self) -> None:
        """Should detect Go from .go extension."""
        assert get_language_from_extension("main.go") == "Go"

    def test_csharp_extension(self) -> None:
        """Should detect C# from .cs extension."""
        assert get_language_from_extension("Program.cs") == "C#"

    def test_cobol_extension(self) -> None:
        """Should detect COBOL from .cbl, .cob, and .cpy extensions."""
        assert get_language_from_extension("PROGRAM.cbl") == "COBOL"
        assert get_language_from_extension("program.cob") == "COBOL"
        assert get_language_from_extension("COPYBOOK.cpy") == "COBOL"

    def test_unknown_extension(self) -> None:
        """Should return empty string for unknown extensions."""
        assert get_language_from_extension("file.xyz") == ""
        assert get_language_from_extension("file.txt") == ""


class TestGetPatternsForLanguage:
    """Tests for pattern retrieval by language."""

    def test_returns_patterns_for_known_language(self) -> None:
        """Should return patterns for known languages."""
        patterns = get_patterns_for_language("Java")
        assert IntegrationType.HTTP_REST in patterns
        assert len(patterns[IntegrationType.HTTP_REST]) > 0

    def test_returns_common_patterns_for_unknown_language(self) -> None:
        """Should return common patterns for unknown languages."""
        patterns = get_patterns_for_language("UnknownLang")
        assert IntegrationType.HTTP_REST in patterns
        # Should have common patterns
        assert len(patterns[IntegrationType.HTTP_REST]) > 0

    def test_rust_patterns_include_framework_imports(self) -> None:
        """Should include Rust-specific framework patterns."""
        patterns = get_patterns_for_language("Rust")
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("actix_web" in p for p in http_patterns)
        assert any("warp" in p for p in http_patterns)


class TestClassifyDirectory:
    """Tests for directory classification."""

    def test_classify_controllers_directory(self) -> None:
        """Should classify 'controllers' as HTTP_REST."""
        matches = classify_directory("controllers")
        types = [m[0] for m in matches]
        assert IntegrationType.HTTP_REST in types

    def test_classify_api_directory(self) -> None:
        """Should classify 'api' as HTTP_REST."""
        matches = classify_directory("api")
        types = [m[0] for m in matches]
        assert IntegrationType.HTTP_REST in types

    def test_classify_repository_directory(self) -> None:
        """Should classify 'repository' as DATABASE."""
        matches = classify_directory("repository")
        types = [m[0] for m in matches]
        assert IntegrationType.DATABASE in types

    def test_classify_models_directory(self) -> None:
        """Should classify 'models' as DATABASE."""
        matches = classify_directory("models")
        types = [m[0] for m in matches]
        assert IntegrationType.DATABASE in types

    def test_classify_kafka_directory(self) -> None:
        """Should classify 'kafka' as MESSAGING."""
        matches = classify_directory("kafka")
        types = [m[0] for m in matches]
        assert IntegrationType.MESSAGING in types

    def test_classify_unknown_directory(self) -> None:
        """Should return empty list for unknown directories."""
        matches = classify_directory("utils")
        assert len(matches) == 0


class TestScanFileForIntegrations:
    """Tests for scanning individual files."""

    def test_scan_java_file_with_spring_annotations(self) -> None:
        """Should detect Spring annotations in Java files."""
        with tempfile.NamedTemporaryFile(
            suffix=".java", mode="w", delete=False
        ) as f:
            f.write("""
@RestController
@RequestMapping("/api/users")
public class UserController {
    @GetMapping("/{id}")
    public User getUser(@PathVariable Long id) {
        return userService.findById(id);
    }
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            http_points = [p for p in points if p.integration_type == IntegrationType.HTTP_REST]
            assert len(http_points) > 0

            # Check for high confidence matches
            high_conf = [p for p in http_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_rust_file_with_actix(self) -> None:
        """Should detect Actix patterns in Rust files."""
        with tempfile.NamedTemporaryFile(
            suffix=".rs", mode="w", delete=False
        ) as f:
            f.write("""
use actix_web::{get, web, HttpResponse};

#[get("/hello")]
async fn hello() -> HttpResponse {
    HttpResponse::Ok().body("Hello!")
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            http_points = [p for p in points if p.integration_type == IntegrationType.HTTP_REST]
            assert len(http_points) > 0

            # Should have high confidence matches for actix imports
            high_conf = [p for p in http_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_python_file_with_flask(self) -> None:
        """Should detect Flask patterns in Python files."""
        with tempfile.NamedTemporaryFile(
            suffix=".py", mode="w", delete=False
        ) as f:
            f.write("""
from flask import Flask, jsonify

app = Flask(__name__)

@app.route('/api/users')
def get_users():
    return jsonify([])
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            http_points = [p for p in points if p.integration_type == IntegrationType.HTTP_REST]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_scan_file_with_database_patterns(self) -> None:
        """Should detect database patterns."""
        with tempfile.NamedTemporaryFile(
            suffix=".java", mode="w", delete=False
        ) as f:
            f.write("""
@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    @Query("SELECT u FROM User u WHERE u.email = :email")
    User findByEmail(@Param("email") String email);
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            db_points = [p for p in points if p.integration_type == IntegrationType.DATABASE]
            assert len(db_points) > 0

            # @Repository should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_file_with_messaging_patterns(self) -> None:
        """Should detect messaging patterns."""
        with tempfile.NamedTemporaryFile(
            suffix=".java", mode="w", delete=False
        ) as f:
            f.write("""
@KafkaListener(topics = "orders")
public void handleOrder(OrderEvent event) {
    processOrder(event);
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            msg_points = [p for p in points if p.integration_type == IntegrationType.MESSAGING]
            assert len(msg_points) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_db2(self) -> None:
        """Should detect DB2 embedded SQL patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(
            suffix=".cbl", mode="w", delete=False
        ) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTREAD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.
           EXEC SQL
               SELECT CUST_NAME, CUST_ADDR
               INTO :WS-CUST-NAME, :WS-CUST-ADDR
               FROM CUSTOMER
               WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.
           IF SQLCODE = 0
               DISPLAY 'CUSTOMER FOUND'
           END-IF.
           STOP RUN.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            db_points = [p for p in points if p.integration_type == IntegrationType.DATABASE]
            assert len(db_points) > 0

            # EXEC SQL should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_mq(self) -> None:
        """Should detect MQ patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(
            suffix=".cbl", mode="w", delete=False
        ) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQSEND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MQOD.
       01  MQMD.
       PROCEDURE DIVISION.
           CALL 'MQOPEN' USING HCONN MQOD MQOO HOBJ CC RC.
           CALL 'MQPUT' USING HCONN HOBJ MQMD MQPMO MSGLENGTH MSGBUFFER CC RC.
           CALL 'MQCLOSE' USING HCONN HOBJ MQCO CC RC.
           STOP RUN.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            msg_points = [p for p in points if p.integration_type == IntegrationType.MESSAGING]
            assert len(msg_points) > 0

            # MQPUT/MQGET should be high confidence
            high_conf = [p for p in msg_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_cics(self) -> None:
        """Should detect CICS patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(
            suffix=".cbl", mode="w", delete=False
        ) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSWEB.
       PROCEDURE DIVISION.
           EXEC CICS WEB SEND
               FROM(WS-RESPONSE)
               FROMLENGTH(WS-RESP-LEN)
               MEDIATYPE('application/json')
           END-EXEC.
           EXEC CICS READ
               FILE('CUSTFILE')
               INTO(WS-CUST-REC)
               RIDFLD(WS-CUST-KEY)
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            # Should detect HTTP/REST (CICS WEB)
            http_points = [p for p in points if p.integration_type == IntegrationType.HTTP_REST]
            assert len(http_points) > 0

            # Should detect DATABASE (CICS READ)
            db_points = [p for p in points if p.integration_type == IntegrationType.DATABASE]
            assert len(db_points) > 0
        finally:
            file_path.unlink()

    def test_scan_file_returns_line_info(self) -> None:
        """Should return correct line number and content."""
        with tempfile.NamedTemporaryFile(
            suffix=".java", mode="w", delete=False
        ) as f:
            f.write("""package com.example;

import org.springframework.web.bind.annotation.*;

@RestController
public class MyController {
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            rest_controller_points = [
                p for p in points if "@RestController" in p.matched_pattern
            ]
            assert len(rest_controller_points) > 0
            point = rest_controller_points[0]
            assert point.match.line_number == 5
            assert "@RestController" in point.match.line_content
        finally:
            file_path.unlink()


class TestDetectIntegrations:
    """Tests for the main detect_integrations function."""

    def test_detect_integrations_returns_result(self) -> None:
        """Should return IntegrationDetectorResult."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a simple Java file
            java_file = Path(tmpdir) / "Test.java"
            java_file.write_text("@RestController\npublic class Test {}")

            result = detect_integrations(tmpdir)

            assert isinstance(result, IntegrationDetectorResult)
            assert result.files_scanned >= 1

    def test_detect_integrations_finds_http_rest(self) -> None:
        """Should detect HTTP/REST integration points."""
        with tempfile.TemporaryDirectory() as tmpdir:
            java_file = Path(tmpdir) / "UserController.java"
            java_file.write_text("""
@RestController
@RequestMapping("/api/users")
public class UserController {
    @GetMapping
    public List<User> getAll() { return null; }
}
""")

            result = detect_integrations(tmpdir)

            http_points = [
                p for p in result.integration_points
                if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) >= 1

    def test_detect_integrations_with_language_filter(self) -> None:
        """Should filter by language when specified."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create files in different languages
            java_file = Path(tmpdir) / "Test.java"
            java_file.write_text("@RestController\npublic class Test {}")

            py_file = Path(tmpdir) / "app.py"
            py_file.write_text("from flask import Flask\napp = Flask(__name__)")

            # Filter to Java only
            result = detect_integrations(tmpdir, languages=["Java"])

            assert result.files_scanned == 1

    def test_detect_integrations_empty_directory(self) -> None:
        """Should handle empty directory."""
        with tempfile.TemporaryDirectory() as tmpdir:
            result = detect_integrations(tmpdir)

            assert result.files_scanned == 0
            assert len(result.integration_points) == 0

    def test_detect_integrations_nonexistent_path(self) -> None:
        """Should handle non-existent path."""
        result = detect_integrations("/nonexistent/path/12345")

        assert result.files_scanned == 0
        assert len(result.integration_points) == 0

    def test_detect_integrations_skips_excluded_dirs(self) -> None:
        """Should skip excluded directories like node_modules."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create file in node_modules
            node_modules = Path(tmpdir) / "node_modules"
            node_modules.mkdir()
            js_file = node_modules / "package.js"
            js_file.write_text("app.get('/api', (req, res) => {});")

            # Create file outside node_modules
            src_file = Path(tmpdir) / "app.js"
            src_file.write_text("app.get('/api', (req, res) => {});")

            result = detect_integrations(tmpdir)

            # Should only scan the file outside node_modules
            assert result.files_scanned == 1

    def test_detect_integrations_includes_directory_matches(self) -> None:
        """Should detect integration points from directory names."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create controllers directory
            controllers = Path(tmpdir) / "controllers"
            controllers.mkdir()
            java_file = controllers / "UserController.java"
            java_file.write_text("public class UserController {}")

            result = detect_integrations(tmpdir)

            dir_points = [
                p for p in result.integration_points
                if p.entity_type == EntityType.DIRECTORY
            ]
            assert len(dir_points) >= 1


class TestIntegrationPointDataclass:
    """Tests for the IntegrationPoint dataclass."""

    def test_integration_point_is_frozen(self) -> None:
        """Should be immutable."""
        match = FileMatch(
            file_path="/test.java",
            line_number=1,
            line_content="test",
            language="Java",
        )
        point = IntegrationPoint(
            match=match,
            integration_type=IntegrationType.HTTP_REST,
            confidence=Confidence.HIGH,
            matched_pattern="@RestController",
            entity_type=EntityType.FILE_CONTENT,
        )

        with pytest.raises(Exception):  # FrozenInstanceError
            point.confidence = Confidence.LOW  # type: ignore


class TestFileMatchDataclass:
    """Tests for the FileMatch dataclass."""

    def test_file_match_is_frozen(self) -> None:
        """Should be immutable."""
        match = FileMatch(
            file_path="/test.java",
            line_number=1,
            line_content="test",
            language="Java",
        )

        with pytest.raises(Exception):  # FrozenInstanceError
            match.line_number = 2  # type: ignore


class TestConfidenceLevels:
    """Tests for confidence level assignment."""

    def test_high_confidence_for_annotations(self) -> None:
        """Annotations should have high confidence."""
        with tempfile.NamedTemporaryFile(
            suffix=".java", mode="w", delete=False
        ) as f:
            f.write("@RestController\npublic class Test {}")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            annotation_points = [
                p for p in points if "@RestController" in p.matched_pattern
            ]
            assert len(annotation_points) > 0
            assert annotation_points[0].confidence == Confidence.HIGH
        finally:
            file_path.unlink()

    def test_low_confidence_for_generic_terms(self) -> None:
        """Generic terms should have low confidence."""
        with tempfile.NamedTemporaryFile(
            suffix=".java", mode="w", delete=False
        ) as f:
            f.write("// This is an API endpoint")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            api_points = [
                p for p in points
                if "(?i)\\bapi\\b" in p.matched_pattern
            ]
            assert len(api_points) > 0
            assert api_points[0].confidence == Confidence.LOW
        finally:
            file_path.unlink()
