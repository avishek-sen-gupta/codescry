"""Tests for integration point detection from file contents."""

import json
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
    IntegrationSignal,
    IntegrationType,
    Language,
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
        assert get_language_from_extension("Test.java") == Language.JAVA
        assert get_language_from_extension("/path/to/Test.java") == Language.JAVA

    def test_rust_extension(self) -> None:
        """Should detect Rust from .rs extension."""
        assert get_language_from_extension("main.rs") == Language.RUST

    def test_python_extension(self) -> None:
        """Should detect Python from .py extension."""
        assert get_language_from_extension("app.py") == Language.PYTHON

    def test_typescript_extension(self) -> None:
        """Should detect TypeScript from .ts and .tsx extensions."""
        assert get_language_from_extension("app.ts") == Language.TYPESCRIPT
        assert get_language_from_extension("component.tsx") == Language.TYPESCRIPT

    def test_javascript_extension(self) -> None:
        """Should detect JavaScript from .js and .jsx extensions."""
        assert get_language_from_extension("app.js") == Language.JAVASCRIPT
        assert get_language_from_extension("component.jsx") == Language.JAVASCRIPT

    def test_go_extension(self) -> None:
        """Should detect Go from .go extension."""
        assert get_language_from_extension("main.go") == Language.GO

    def test_csharp_extension(self) -> None:
        """Should detect C# from .cs extension."""
        assert get_language_from_extension("Program.cs") == Language.CSHARP

    def test_cobol_extension(self) -> None:
        """Should detect COBOL from .cbl, .cob, and .cpy extensions."""
        assert get_language_from_extension("PROGRAM.cbl") == Language.COBOL
        assert get_language_from_extension("program.cob") == Language.COBOL
        assert get_language_from_extension("COPYBOOK.cpy") == Language.COBOL

    def test_pli_extension(self) -> None:
        """Should detect PL/I from .pli and .pl1 extensions."""
        assert get_language_from_extension("PROGRAM.pli") == Language.PLI
        assert get_language_from_extension("program.pl1") == Language.PLI
        assert get_language_from_extension("include.plinc") == Language.PLI

    def test_unknown_extension(self) -> None:
        """Should return None for unknown extensions."""
        assert get_language_from_extension("file.xyz") is None
        assert get_language_from_extension("file.txt") is None


class TestGetPatternsForLanguage:
    """Tests for pattern retrieval by language."""

    def test_returns_patterns_for_known_language(self) -> None:
        """Should return patterns for known languages."""
        patterns = get_patterns_for_language(Language.JAVA, frameworks=["Spring"])
        assert IntegrationType.HTTP_REST in patterns
        assert len(patterns[IntegrationType.HTTP_REST]) > 0

    def test_returns_common_patterns_for_none_language(self) -> None:
        """Should return common patterns when language is None."""
        patterns = get_patterns_for_language(None)
        assert IntegrationType.HTTP_REST in patterns
        # Should have common patterns
        assert len(patterns[IntegrationType.HTTP_REST]) > 0

    def test_rust_patterns_include_framework_imports(self) -> None:
        """Should include Rust-specific framework patterns when frameworks active."""
        patterns = get_patterns_for_language(
            Language.RUST, frameworks=["Actix", "Warp"]
        )
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
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
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
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            assert len(points) > 0

            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0

            # Check for high confidence matches
            high_conf = [p for p in http_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_rust_file_with_actix(self) -> None:
        """Should detect Actix patterns in Rust files."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
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
            points = list(scan_file_for_integrations(file_path, frameworks=["Actix"]))
            assert len(points) > 0

            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0

            # Should have high confidence matches for actix imports
            high_conf = [p for p in http_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_python_file_with_flask(self) -> None:
        """Should detect Flask patterns in Python files."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
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

            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_scan_file_with_database_patterns(self) -> None:
        """Should detect database patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
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
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # @Repository should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_file_with_messaging_patterns(self) -> None:
        """Should detect messaging patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("""
@KafkaListener(topics = "orders")
public void handleOrder(OrderEvent event) {
    processOrder(event);
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            msg_points = [
                p for p in points if p.integration_type == IntegrationType.MESSAGING
            ]
            assert len(msg_points) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_db2(self) -> None:
        """Should detect DB2 embedded SQL patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
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

            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # EXEC SQL should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_mq(self) -> None:
        """Should detect MQ patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
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

            msg_points = [
                p for p in points if p.integration_type == IntegrationType.MESSAGING
            ]
            assert len(msg_points) > 0

            # MQPUT/MQGET should be high confidence
            high_conf = [p for p in msg_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_idms(self) -> None:
        """Should detect IDMS patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDMSREAD.
       DATA DIVISION.
       SCHEMA SECTION.
           COPY IDMS SUBSCHEMA-DESCRIPTION.
       WORKING-STORAGE SECTION.
       01  SUBSCHEMA-CTRL.
       01  IDMS-STATUS.
       PROCEDURE DIVISION.
           BIND RUN-UNIT.
           READY EMPLOYEE-AREA USAGE-MODE UPDATE.
           OBTAIN CALC EMPLOYEE.
           IF IDMS-STATUS = '0000'
               COMMIT TASK
           ELSE
               ROLLBACK TASK
           END-IF.
           FINISH TASK.
           STOP RUN.
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # BIND RUN-UNIT, OBTAIN CALC, and SUBSCHEMA-CTRL should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0

            # Check for specific IDMS patterns
            patterns = [p.matched_pattern for p in db_points]
            assert any(
                "BIND" in p or "OBTAIN" in p or "SUBSCHEMA-CTRL" in p for p in patterns
            )
        finally:
            file_path.unlink()

    def test_scan_pli_file_with_db2(self) -> None:
        """Should detect DB2 embedded SQL patterns in PL/I files."""
        with tempfile.NamedTemporaryFile(suffix=".pli", mode="w", delete=False) as f:
            f.write("""CUSTREAD: PROC OPTIONS(MAIN);
   DCL SQLCA EXTERNAL;
   %INCLUDE SQLCA;

   EXEC SQL
       SELECT CUST_NAME, CUST_ADDR
       INTO :WS_CUST_NAME, :WS_CUST_ADDR
       FROM CUSTOMER
       WHERE CUST_ID = :WS_CUST_ID;

   IF SQLCODE = 0 THEN
       PUT SKIP LIST('CUSTOMER FOUND');
   END;
END CUSTREAD;
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0

            # EXEC SQL and SQLCA should be high confidence
            high_conf = [p for p in db_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_pli_file_with_mq(self) -> None:
        """Should detect MQ patterns in PL/I files."""
        with tempfile.NamedTemporaryFile(suffix=".pli", mode="w", delete=False) as f:
            f.write("""MQSEND: PROC OPTIONS(MAIN);
   %INCLUDE CMQP;

   CALL MQCONN(QMGR_NAME, HCONN, CC, RC);
   CALL MQOPEN(HCONN, MQOD, MQOO_OUTPUT, HOBJ, CC, RC);
   CALL MQPUT(HCONN, HOBJ, MQMD, MQPMO, MSG_LEN, MSG_BUFFER, CC, RC);
   CALL MQCLOSE(HCONN, HOBJ, MQCO_NONE, CC, RC);
   CALL MQDISC(HCONN, CC, RC);

END MQSEND;
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            assert len(points) > 0

            msg_points = [
                p for p in points if p.integration_type == IntegrationType.MESSAGING
            ]
            assert len(msg_points) > 0

            # CALL MQPUT/MQGET should be high confidence
            high_conf = [p for p in msg_points if p.confidence == Confidence.HIGH]
            assert len(high_conf) > 0
        finally:
            file_path.unlink()

    def test_scan_cobol_file_with_cics(self) -> None:
        """Should detect CICS patterns in COBOL files."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
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
            http_points = [
                p for p in points if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) > 0

            # Should detect DATABASE (CICS READ)
            db_points = [
                p for p in points if p.integration_type == IntegrationType.DATABASE
            ]
            assert len(db_points) > 0
        finally:
            file_path.unlink()

    def test_scan_file_returns_line_info(self) -> None:
        """Should return correct line number and content."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("""package com.example;

import org.springframework.web.bind.annotation.*;

@RestController
public class MyController {
}
""")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
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

            result = detect_integrations(tmpdir, directory_frameworks={".": ["Spring"]})

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

            result = detect_integrations(tmpdir, directory_frameworks={".": ["Spring"]})

            http_points = [
                p
                for p in result.integration_points
                if p.integration_type == IntegrationType.HTTP_REST
            ]
            assert len(http_points) >= 1

    def test_detect_integrations_with_language_filter(self) -> None:
        """Should filter by language when specified."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create files in different languages using base patterns
            java_file = Path(tmpdir) / "Test.java"
            java_file.write_text("@Entity\npublic class Test {}")

            py_file = Path(tmpdir) / "app.py"
            py_file.write_text("import requests\n")

            # Filter to Java only
            result = detect_integrations(tmpdir, languages=[Language.JAVA])

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
                p
                for p in result.integration_points
                if p.entity_type == EntityType.DIRECTORY
            ]
            assert len(dir_points) >= 1


class TestIntegrationSignalDataclass:
    """Tests for the IntegrationSignal dataclass."""

    def test_integration_point_is_frozen(self) -> None:
        """Should be immutable."""
        match = FileMatch(
            file_path="/test.java",
            line_number=1,
            line_content="test",
            language=Language.JAVA,
        )
        point = IntegrationSignal(
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
            language=Language.JAVA,
        )

        with pytest.raises(Exception):  # FrozenInstanceError
            match.line_number = 2  # type: ignore


class TestConfidenceLevels:
    """Tests for confidence level assignment."""

    def test_high_confidence_for_annotations(self) -> None:
        """Annotations should have high confidence."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("@RestController\npublic class Test {}")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Spring"]))
            annotation_points = [
                p for p in points if "@RestController" in p.matched_pattern
            ]
            assert len(annotation_points) > 0
            assert annotation_points[0].confidence == Confidence.HIGH
        finally:
            file_path.unlink()

    def test_low_confidence_for_generic_terms(self) -> None:
        """Generic terms should have low confidence."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("// This is an API endpoint")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            api_points = [p for p in points if "(?i)\\bapi\\b" in p.matched_pattern]
            assert len(api_points) > 0
            assert api_points[0].confidence == Confidence.LOW
        finally:
            file_path.unlink()


class TestFrameworkAwarePatterns:
    """Tests for framework-specific pattern matching."""

    def test_flask_patterns_only_with_flask_framework(self) -> None:
        """Flask-specific patterns should only match when Flask framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("from flask import Flask\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Flask framework: should NOT match flask-specific pattern
            points_without = list(scan_file_for_integrations(file_path))
            flask_import_points = [
                p for p in points_without if "from flask import" in p.matched_pattern
            ]
            assert len(flask_import_points) == 0

            # With Flask framework: should match
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Flask"])
            )
            flask_import_points = [
                p for p in points_with if "from flask import" in p.matched_pattern
            ]
            assert len(flask_import_points) > 0
            assert flask_import_points[0].confidence == Confidence.HIGH
        finally:
            file_path.unlink()

    def test_fastapi_patterns_only_with_fastapi_framework(self) -> None:
        """FastAPI-specific patterns should only match when FastAPI framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("from fastapi import FastAPI\n@app.get('/users')\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without FastAPI framework
            points_without = list(scan_file_for_integrations(file_path))
            fastapi_points = [
                p
                for p in points_without
                if "from fastapi import" in p.matched_pattern
                or r"@\w+\.get" in p.matched_pattern
            ]
            assert len(fastapi_points) == 0

            # With FastAPI framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["FastAPI"])
            )
            fastapi_points = [
                p
                for p in points_with
                if "from fastapi import" in p.matched_pattern
                or r"@\w+\.get" in p.matched_pattern
            ]
            assert len(fastapi_points) > 0
        finally:
            file_path.unlink()

    def test_django_patterns_only_with_django_framework(self) -> None:
        """Django-specific patterns should only match when Django framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write(
                "from django.db import models\nclass User(models.Model):\n    pass\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Django framework
            points_without = list(scan_file_for_integrations(file_path))
            django_db_points = [
                p
                for p in points_without
                if "from django" in p.matched_pattern
                or r"models\.Model" in p.matched_pattern
            ]
            assert len(django_db_points) == 0

            # With Django framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Django"])
            )
            django_db_points = [
                p
                for p in points_with
                if "from django" in p.matched_pattern
                or r"models\.Model" in p.matched_pattern
            ]
            assert len(django_db_points) > 0
        finally:
            file_path.unlink()

    def test_base_python_patterns_always_match(self) -> None:
        """Base language patterns should match regardless of framework."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("from sqlalchemy import create_engine\nimport requests\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without any framework
            points = list(scan_file_for_integrations(file_path))
            sqlalchemy_points = [
                p for p in points if "from sqlalchemy import" in p.matched_pattern
            ]
            requests_points = [
                p for p in points if "import requests" in p.matched_pattern
            ]
            assert len(sqlalchemy_points) > 0
            assert len(requests_points) > 0

            # With a framework - base patterns should still match
            points_with_fw = list(
                scan_file_for_integrations(file_path, frameworks=["Flask"])
            )
            sqlalchemy_points = [
                p
                for p in points_with_fw
                if "from sqlalchemy import" in p.matched_pattern
            ]
            assert len(sqlalchemy_points) > 0
        finally:
            file_path.unlink()

    def test_nestjs_patterns_only_with_nestjs_framework(self) -> None:
        """NestJS-specific patterns should only match when NestJS framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".ts", mode="w", delete=False) as f:
            f.write("@Controller('users')\n@Get()\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without NestJS framework
            points_without = list(scan_file_for_integrations(file_path))
            nestjs_points = [
                p
                for p in points_without
                if r"@Controller\(" in p.matched_pattern
                or r"@Get\(" in p.matched_pattern
            ]
            assert len(nestjs_points) == 0

            # With NestJS framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["NestJS"])
            )
            nestjs_points = [
                p
                for p in points_with
                if r"@Controller\(" in p.matched_pattern
                or r"@Get\(" in p.matched_pattern
            ]
            assert len(nestjs_points) > 0
        finally:
            file_path.unlink()

    def test_express_js_patterns_only_with_express_framework(self) -> None:
        """Express-specific patterns should only match when Express framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".js", mode="w", delete=False) as f:
            f.write("const express = require('express');\napp.get('/api', handler);\n")
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Express framework
            points_without = list(scan_file_for_integrations(file_path))
            express_points = [
                p
                for p in points_without
                if "require" in p.matched_pattern and "express" in p.matched_pattern
            ]
            assert len(express_points) == 0

            # With Express framework
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Express"])
            )
            express_points = [
                p
                for p in points_with
                if "require" in p.matched_pattern and "express" in p.matched_pattern
            ]
            assert len(express_points) > 0
        finally:
            file_path.unlink()

    def test_multiple_frameworks_combine_patterns(self) -> None:
        """Multiple active frameworks should combine their patterns."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("from flask import Flask\nfrom django.db import models\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["Flask", "Django"])
            )
            flask_points = [
                p for p in points if "from flask import" in p.matched_pattern
            ]
            django_points = [p for p in points if "from django" in p.matched_pattern]
            assert len(flask_points) > 0
            assert len(django_points) > 0
        finally:
            file_path.unlink()

    def test_unrecognised_framework_is_ignored(self) -> None:
        """An unrecognised framework name should not cause errors."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("import requests\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(
                    file_path, frameworks=["NonExistentFramework"]
                )
            )
            requests_points = [
                p for p in points if "import requests" in p.matched_pattern
            ]
            assert len(requests_points) > 0
        finally:
            file_path.unlink()


class TestGetPatternsForLanguageWithFrameworks:
    """Tests for get_patterns_for_language with framework parameter."""

    def test_no_frameworks_returns_base_only(self) -> None:
        """Without frameworks, should return only common + base patterns."""
        patterns = get_patterns_for_language(Language.PYTHON)
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        # Base pattern should be present
        assert any("import requests" in p for p in http_patterns)
        # Framework-specific pattern should NOT be present
        assert not any("from flask import" in p for p in http_patterns)
        assert not any("from fastapi import" in p for p in http_patterns)

    def test_flask_framework_adds_flask_patterns(self) -> None:
        """With Flask framework, should include Flask-specific patterns."""
        patterns = get_patterns_for_language(Language.PYTHON, frameworks=["Flask"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any("from flask import" in p for p in http_patterns)
        assert any(r"@\w+\.route" in p for p in http_patterns)
        # Base should still be there
        assert any("import requests" in p for p in http_patterns)

    def test_django_framework_adds_database_patterns(self) -> None:
        """With Django framework, should include Django database patterns."""
        patterns = get_patterns_for_language(Language.PYTHON, frameworks=["Django"])
        db_patterns = [p[0] for p in patterns[IntegrationType.DATABASE]]
        assert any(r"from django\.db import" in p for p in db_patterns)
        assert any(r"models\.Model" in p for p in db_patterns)
        # Base database patterns should still be there
        assert any("from sqlalchemy import" in p for p in db_patterns)

    def test_nestjs_framework_adds_nestjs_patterns(self) -> None:
        """With NestJS framework, should include NestJS-specific patterns."""
        patterns = get_patterns_for_language(Language.TYPESCRIPT, frameworks=["NestJS"])
        http_patterns = [p[0] for p in patterns[IntegrationType.HTTP_REST]]
        assert any(r"@Controller\(" in p for p in http_patterns)
        assert any(r"@Get\(" in p for p in http_patterns)

    def test_none_language_ignores_frameworks(self) -> None:
        """With None language, frameworks parameter should be ignored."""
        patterns_without = get_patterns_for_language(None)
        patterns_with = get_patterns_for_language(None, frameworks=["Flask"])
        assert patterns_without == patterns_with


class TestDetectIntegrationsWithFrameworks:
    """Tests for detect_integrations with directory_frameworks."""

    def test_framework_patterns_applied_per_directory(self) -> None:
        """Framework-specific patterns should apply based on directory mapping."""
        with tempfile.TemporaryDirectory() as tmpdir:
            py_file = Path(tmpdir) / "app.py"
            py_file.write_text("from flask import Flask\n@app.route('/hello')\n")

            result = detect_integrations(
                tmpdir,
                directory_frameworks={".": ["Flask"]},
            )

            flask_points = [
                p
                for p in result.integration_points
                if p.entity_type == EntityType.FILE_CONTENT
                and (
                    "from flask import" in p.matched_pattern
                    or r"@\w+\.route" in p.matched_pattern
                )
            ]
            assert len(flask_points) > 0

    def test_no_framework_patterns_without_mapping(self) -> None:
        """Without directory_frameworks, framework-specific patterns should not match."""
        with tempfile.TemporaryDirectory() as tmpdir:
            py_file = Path(tmpdir) / "app.py"
            py_file.write_text("from flask import Flask\n@app.route('/hello')\n")

            result = detect_integrations(tmpdir)

            flask_specific_points = [
                p
                for p in result.integration_points
                if p.entity_type == EntityType.FILE_CONTENT
                and (
                    "from flask import" in p.matched_pattern
                    or r"@\w+\.route" in p.matched_pattern
                )
            ]
            assert len(flask_specific_points) == 0

    def test_subdirectory_inherits_parent_frameworks(self) -> None:
        """Files in subdirectories should inherit frameworks from parent directories."""
        with tempfile.TemporaryDirectory() as tmpdir:
            sub_dir = Path(tmpdir) / "api"
            sub_dir.mkdir()
            py_file = sub_dir / "routes.py"
            py_file.write_text("from fastapi import FastAPI\n@app.get('/users')\n")

            result = detect_integrations(
                tmpdir,
                directory_frameworks={".": ["FastAPI"]},
            )

            fastapi_points = [
                p
                for p in result.integration_points
                if p.entity_type == EntityType.FILE_CONTENT
                and (
                    "from fastapi import" in p.matched_pattern
                    or r"@\w+\.get" in p.matched_pattern
                )
            ]
            assert len(fastapi_points) > 0

    def test_different_frameworks_per_directory(self) -> None:
        """Different directories can have different frameworks."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Backend with FastAPI
            backend = Path(tmpdir) / "backend"
            backend.mkdir()
            backend_file = backend / "main.py"
            backend_file.write_text("from fastapi import FastAPI\n")

            # Frontend API with Flask
            frontend = Path(tmpdir) / "frontend"
            frontend.mkdir()
            frontend_file = frontend / "app.py"
            frontend_file.write_text("from flask import Flask\n")

            result = detect_integrations(
                tmpdir,
                directory_frameworks={
                    "backend": ["FastAPI"],
                    "frontend": ["Flask"],
                },
            )

            # FastAPI pattern should match in backend
            fastapi_points = [
                p
                for p in result.integration_points
                if "from fastapi import" in p.matched_pattern
            ]
            assert len(fastapi_points) > 0
            assert "backend" in fastapi_points[0].match.file_path

            # Flask pattern should match in frontend
            flask_points = [
                p
                for p in result.integration_points
                if "from flask import" in p.matched_pattern
            ]
            assert len(flask_points) > 0
            assert "frontend" in flask_points[0].match.file_path


class TestJavalinFrameworkPatterns:
    """Tests for Javalin framework-specific pattern matching."""

    def test_javalin_patterns_only_with_javalin_framework(self) -> None:
        """Javalin-specific patterns should only match when Javalin framework is active."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n"
                "var app = Javalin.create();\n"
                'app.get("/hello", ctx -> ctx.result("Hello"));\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            # Without Javalin framework: should NOT match javalin-specific patterns
            points_without = list(scan_file_for_integrations(file_path))
            javalin_points = [
                p
                for p in points_without
                if "import io\\.javalin" in p.matched_pattern
                or "Javalin\\.create" in p.matched_pattern
            ]
            assert len(javalin_points) == 0

            # With Javalin framework: should match
            points_with = list(
                scan_file_for_integrations(file_path, frameworks=["Javalin"])
            )
            javalin_points = [
                p
                for p in points_with
                if "import io\\.javalin" in p.matched_pattern
                or "Javalin\\.create" in p.matched_pattern
            ]
            assert len(javalin_points) > 0
            assert all(p.confidence == Confidence.HIGH for p in javalin_points)
        finally:
            file_path.unlink()

    def test_javalin_route_patterns(self) -> None:
        """Javalin route handler patterns should match with Javalin framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                'app.get("/users", ctx -> ctx.json(users));\n'
                'app.post("/users", ctx -> { });\n'
                'app.delete("/users/{id}", ctx -> { });\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            matched_patterns = {p.matched_pattern for p in http_points}
            assert r"\w+\.get\(" in matched_patterns
            assert r"\w+\.post\(" in matched_patterns
            assert r"\w+\.delete\(" in matched_patterns
        finally:
            file_path.unlink()

    def test_javalin_websocket_patterns(self) -> None:
        """Javalin WebSocket patterns should match with Javalin framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write('app.ws("/websocket", ws -> { });\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            ws_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOCKET
                and r"\w+\.ws\(" in p.matched_pattern
            ]
            assert len(ws_points) > 0
        finally:
            file_path.unlink()

    def test_javalin_base_patterns_still_match(self) -> None:
        """Base Java patterns should still match alongside Javalin patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.javalin.Javalin;\n" "@Entity\n" "public class User { }\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Javalin"]))
            javalin_points = [
                p for p in points if "import io\\.javalin" in p.matched_pattern
            ]
            entity_points = [p for p in points if "@Entity" in p.matched_pattern]
            assert len(javalin_points) > 0
            assert len(entity_points) > 0
        finally:
            file_path.unlink()


class TestFileIoPatterns:
    """Tests for FILE_IO integration type detection."""

    def test_java_file_input_stream(self) -> None:
        """Should detect FileInputStream as FILE_IO."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("FileInputStream fis = new FileInputStream(path);\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_python_open(self) -> None:
        """Should detect open() as FILE_IO."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("with open('data.txt') as f:\n    data = f.read()\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and "open\\(" in p.matched_pattern
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_python_pathlib(self) -> None:
        """Should detect pathlib.Path as FILE_IO."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("p = pathlib.Path('/tmp/data.txt')\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and "pathlib" in p.matched_pattern
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_cobol_open_input(self) -> None:
        """Should detect OPEN INPUT as FILE_IO in COBOL."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("       OPEN INPUT CUSTOMER-FILE.\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_cobol_open_output(self) -> None:
        """Should detect OPEN OUTPUT as FILE_IO in COBOL."""
        with tempfile.NamedTemporaryFile(suffix=".cbl", mode="w", delete=False) as f:
            f.write("       OPEN OUTPUT REPORT-FILE.\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_go_os_open(self) -> None:
        """Should detect os.Open as FILE_IO in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('f, err := os.Open("data.txt")\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()

    def test_pli_open_file(self) -> None:
        """Should detect OPEN FILE as FILE_IO in PL/I."""
        with tempfile.NamedTemporaryFile(suffix=".pli", mode="w", delete=False) as f:
            f.write("OPEN FILE(CUSTFILE) INPUT;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            file_io_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.FILE_IO
                and p.confidence == Confidence.HIGH
            ]
            assert len(file_io_points) > 0
        finally:
            file_path.unlink()


class TestGrpcPatterns:
    """Tests for GRPC integration type detection."""

    def test_java_grpc_import(self) -> None:
        """Should detect io.grpc import as GRPC in Java."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("import io.grpc.ManagedChannel;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_python_grpc_import(self) -> None:
        """Should detect import grpc as GRPC in Python."""
        with tempfile.NamedTemporaryFile(suffix=".py", mode="w", delete=False) as f:
            f.write("import grpc\nserver = grpc.server(futures.ThreadPoolExecutor())\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_go_grpc_import(self) -> None:
        """Should detect google.golang.org/grpc as GRPC in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('"google.golang.org/grpc"\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_csharp_grpc_core(self) -> None:
        """Should detect Grpc.Core as GRPC in C#."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("using Grpc.Core;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_rust_tonic_grpc(self) -> None:
        """Should detect tonic:: as GRPC in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write("use tonic::transport::Server;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and p.confidence == Confidence.HIGH
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()


class TestDropwizardFrameworkPatterns:
    """Tests for Dropwizard framework-specific pattern matching."""

    def test_dropwizard_http_rest_patterns(self) -> None:
        """Should detect Dropwizard HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.dropwizard.Application;\n"
                '@Path("/users")\n'
                "@Produces(MediaType.APPLICATION_JSON)\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["Dropwizard"])
            )
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
            matched = {p.matched_pattern for p in http_points}
            assert r"import io\.dropwizard" in matched
        finally:
            file_path.unlink()

    def test_dropwizard_patterns_only_with_framework(self) -> None:
        """Dropwizard patterns should not match without framework."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("import io.dropwizard.Application;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            dropwizard_points = [
                p for p in points if "import io\\.dropwizard" in p.matched_pattern
            ]
            assert len(dropwizard_points) == 0
        finally:
            file_path.unlink()


class TestVertxFrameworkPatterns:
    """Tests for Vert.x framework-specific pattern matching."""

    def test_vertx_http_rest_patterns(self) -> None:
        """Should detect Vert.x HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write(
                "import io.vertx.core.Vertx;\n"
                "vertx.createHttpServer();\n"
                "Router.router(vertx);\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Vert.x"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_vertx_socket_patterns(self) -> None:
        """Should detect Vert.x WebSocket patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("ServerWebSocket ws = conn.webSocket();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Vert.x"]))
            socket_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOCKET
                and "ServerWebSocket" in p.matched_pattern
            ]
            assert len(socket_points) > 0
        finally:
            file_path.unlink()

    def test_vertx_messaging_patterns(self) -> None:
        """Should detect Vert.x EventBus patterns."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("EventBus eb = vertx.eventBus();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Vert.x"]))
            msg_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.MESSAGING
                and p.confidence == Confidence.HIGH
            ]
            assert len(msg_points) > 0
        finally:
            file_path.unlink()


class TestCsharpFrameworkPatterns:
    """Tests for C# framework-specific pattern matching."""

    def test_aspnet_core_http_rest(self) -> None:
        """Should detect ASP.NET Core HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "var builder = WebApplication.CreateBuilder(args);\n"
                'app.MapGet("/hello", () => "Hello World");\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["ASP.NET Core"])
            )
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_aspnet_core_grpc(self) -> None:
        """Should detect ASP.NET Core gRPC patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("app.MapGrpcService<GreeterService>();\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["ASP.NET Core"])
            )
            grpc_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.GRPC
                and "MapGrpcService" in p.matched_pattern
            ]
            assert len(grpc_points) > 0
        finally:
            file_path.unlink()

    def test_wcf_soap(self) -> None:
        """Should detect WCF SOAP patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using System.ServiceModel;\n"
                "var host = new ServiceHost(typeof(MyService));\n"
                "var binding = new BasicHttpBinding();\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["WCF"]))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and p.confidence == Confidence.HIGH
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_corewcf_soap(self) -> None:
        """Should detect CoreWCF SOAP patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("using CoreWCF;\nusing CoreWCF.Http;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["CoreWCF"]))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and p.confidence == Confidence.HIGH
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_servicestack_http_rest(self) -> None:
        """Should detect ServiceStack HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                "using ServiceStack;\npublic class MyRequest : IReturn<MyResponse> {}\n"
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(
                scan_file_for_integrations(file_path, frameworks=["ServiceStack"])
            )
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_nancy_http_rest(self) -> None:
        """Should detect Nancy HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write(
                'public class MyModule : NancyModule {\n    Get["/"] = _ => "Hello";\n}\n'
            )
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Nancy"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and p.confidence == Confidence.HIGH
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()

    def test_carter_http_rest(self) -> None:
        """Should detect Carter HTTP/REST patterns."""
        with tempfile.NamedTemporaryFile(suffix=".cs", mode="w", delete=False) as f:
            f.write("public class MyModule : ICarterModule {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path, frameworks=["Carter"]))
            http_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.HTTP_REST
                and "ICarterModule" in p.matched_pattern
            ]
            assert len(http_points) > 0
        finally:
            file_path.unlink()


class TestGoSoapPatterns:
    """Tests for Go SOAP patterns (gap fill)."""

    def test_go_gowsdl(self) -> None:
        """Should detect gowsdl import as SOAP in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('"github.com/hooklift/gowsdl"\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and p.confidence == Confidence.HIGH
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_go_encoding_xml(self) -> None:
        """Should detect encoding/xml as SOAP in Go."""
        with tempfile.NamedTemporaryFile(suffix=".go", mode="w", delete=False) as f:
            f.write('"encoding/xml"\n')
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and "encoding/xml" in p.matched_pattern
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()


class TestRustSoapPatterns:
    """Tests for Rust SOAP patterns (gap fill)."""

    def test_rust_yaserde(self) -> None:
        """Should detect yaserde as SOAP in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write("use yaserde::YaDeserialize;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and p.confidence == Confidence.HIGH
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()

    def test_rust_quick_xml(self) -> None:
        """Should detect quick-xml as SOAP in Rust."""
        with tempfile.NamedTemporaryFile(suffix=".rs", mode="w", delete=False) as f:
            f.write("use quick_xml::Reader;\n")
            f.flush()
            file_path = Path(f.name)

        try:
            points = list(scan_file_for_integrations(file_path))
            soap_points = [
                p
                for p in points
                if p.integration_type == IntegrationType.SOAP
                and p.confidence == Confidence.HIGH
            ]
            assert len(soap_points) > 0
        finally:
            file_path.unlink()


class TestDirectoryClassificationNewTypes:
    """Tests for directory classification with FILE_IO and GRPC types."""

    def test_classify_uploads_directory(self) -> None:
        """Should classify 'uploads' as FILE_IO."""
        matches = classify_directory("uploads")
        types = [m[0] for m in matches]
        assert IntegrationType.FILE_IO in types

    def test_classify_storage_directory(self) -> None:
        """Should classify 'storage' as FILE_IO."""
        matches = classify_directory("storage")
        types = [m[0] for m in matches]
        assert IntegrationType.FILE_IO in types

    def test_classify_proto_directory(self) -> None:
        """Should classify 'proto' as GRPC."""
        matches = classify_directory("proto")
        types = [m[0] for m in matches]
        assert IntegrationType.GRPC in types

    def test_classify_grpc_directory(self) -> None:
        """Should classify 'grpc' as GRPC."""
        matches = classify_directory("grpc")
        types = [m[0] for m in matches]
        assert IntegrationType.GRPC in types


class TestIntegrationDetectorResultJson:
    """Tests for IntegrationDetectorResult.to_json()."""

    def test_to_json_returns_valid_json(self) -> None:
        """to_json() should return parseable JSON with correct structure."""
        with tempfile.NamedTemporaryFile(suffix=".java", mode="w", delete=False) as f:
            f.write("@Entity\npublic class User {}\n")
            f.flush()
            file_path = Path(f.name)

        try:
            result = detect_integrations(str(file_path.parent))
            parsed = json.loads(result.to_json())

            assert "files_scanned" in parsed
            assert "integration_points" in parsed
            assert isinstance(parsed["files_scanned"], int)
            assert isinstance(parsed["integration_points"], list)
        finally:
            file_path.unlink()

    def test_to_json_point_fields(self) -> None:
        """Each integration point should have all expected fields."""
        match = FileMatch(
            file_path="/test.java",
            line_number=5,
            line_content="@RestController",
            language=Language.JAVA,
        )
        signal = IntegrationSignal(
            match=match,
            integration_type=IntegrationType.HTTP_REST,
            confidence=Confidence.HIGH,
            matched_pattern="@RestController",
            entity_type=EntityType.FILE_CONTENT,
        )
        result = IntegrationDetectorResult(
            integration_points=[signal],
            files_scanned=1,
        )
        parsed = json.loads(result.to_json())

        point = parsed["integration_points"][0]
        assert point["integration_type"] == "http_rest"
        assert point["confidence"] == "high"
        assert point["matched_pattern"] == "@RestController"
        assert point["entity_type"] == "file_content"
        assert point["match"]["file_path"] == "/test.java"
        assert point["match"]["line_number"] == 5
        assert point["match"]["line_content"] == "@RestController"
        assert point["match"]["language"] == "Java"

    def test_to_json_empty_result(self) -> None:
        """to_json() should handle an empty result."""
        result = IntegrationDetectorResult(
            integration_points=[],
            files_scanned=0,
        )
        parsed = json.loads(result.to_json())

        assert parsed["files_scanned"] == 0
        assert parsed["integration_points"] == []

    def test_to_json_respects_indent(self) -> None:
        """to_json() should respect the indent parameter."""
        result = IntegrationDetectorResult(
            integration_points=[],
            files_scanned=0,
        )
        compact = result.to_json(indent=None)
        indented = result.to_json(indent=4)

        assert "\n" not in compact
        assert "\n" in indented

    def test_to_json_none_language(self) -> None:
        """to_json() should handle None language gracefully."""
        match = FileMatch(
            file_path="/unknown.xyz",
            line_number=1,
            line_content="http endpoint",
            language=None,
        )
        signal = IntegrationSignal(
            match=match,
            integration_type=IntegrationType.HTTP_REST,
            confidence=Confidence.LOW,
            matched_pattern="(?i)\\bhttp\\b",
            entity_type=EntityType.FILE_CONTENT,
        )
        result = IntegrationDetectorResult(
            integration_points=[signal],
            files_scanned=1,
        )
        parsed = json.loads(result.to_json())

        assert parsed["integration_points"][0]["match"]["language"] is None
